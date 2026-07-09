# seqNdisplayR Nomenclature

Standard terminology for annotation display logic. Use these terms consistently
in code comments, documentation, and discussion.

## Feature types

| Term | Definition |
|------|------------|
| **feature** | Any annotated genomic element (gene, snoRNA, lncRNA, miRNA, etc.). The generic unit of annotation display. Replaces informal use of "gene" when the element may not be a protein-coding gene. |
| **host** | A feature that genomically contains one or more other features. Detected by `.find.hosts()`. A host can itself be enclosed in another host (nesting). |
| **enclosed** | A feature whose genomic span falls entirely within a host. |
| **top-level** | A feature that is not enclosed in any host. |
| **single-transcript** | A feature with exactly 1 transcript. Displayed as a single bar in expanded mode. Replaces "small gene". |
| **multi-transcript** | A feature with 2 or more transcripts. Displayed as multiple stacked rows in expanded mode. Replaces "large gene". |

## Packing and row terminology

| Term | Definition |
|------|------------|
| **packing** | The row assignment of transcripts for a given annotation in expanded/squished mode. |
| **Rmin** | The minimum number of rows needed to pack all top-level features (after greedy row packing of all non-enclosed features). |
| **host row span** | The number of rows a host's own transcripts occupy in expanded packing. |

## Display modes

| Term | Definition |
|------|------------|
| **expanded** | Full transcript-level display. Each transcript drawn individually. |
| **squished** | Like expanded but with reduced vertical spacing. |
| **collapsed2** | Single-bar-per-feature display. Features drawn as merged bars; names placed by ILP. |

## Enclosed feature packing strategy (expanded mode)

When a host contains enclosed features in expanded mode, the display strategy
depends on whether collapsing saves space:

- **If enclosed features fit within available rows** (i.e., the number of
  enclosed features does not exceed the host's row span or Rmin): display each
  enclosed feature on its own row, one per row.
- **If enclosed features exceed available rows** (e.g., TAF1D with 9 snoRNAs):
  use collapsed2-style paired L/R packing to minimize extra rows.
