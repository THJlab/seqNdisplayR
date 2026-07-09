# Annotation Row Packing — Design Document

## Problem

In `expanded` annotation packing mode, non-overlapping genes are stacked
sequentially even when their transcripts could share rows. This wastes
vertical space, especially at loci with many small hosted genes (e.g.,
snoRNAs inside NOP56).

Example — NOP56 locus, gencode v38, expanded mode:
- NOP56 has ~12 transcripts → packed into ~10 rows (good)
- MIR1292, SNORD110, SNORA51, SNORD86, SNORD56, SNORD57 each have 1-2
  tiny transcripts → each gets its own row (bad)
- Total: ~16 rows when ~11 would suffice (NOP56 transcripts + 1 row for
  all snoRNAs)

The `collapsed2` mode already handles this well — it packs gene-level
footprints using OrganizeOverlappingIVs. The expanded mode should do the
same but at the transcript level, with the constraint that transcripts
from the same gene stay grouped.

## Current Architecture

```
AnnotatedFeaturesInRegion()
  → per gene: OrganizeOverlappingIVs(gene_transcripts) → packing
  
OrganizeOverlappingLoci()
  → for overlapping gene groups: merge expanded + re-pack
  → non-overlapping genes: left as separate entries, sequentially stacked
  
PlotAnnotation()
  → for each feature name in .subset.annotation:
      for each .pack.line in .packing[[.feat.name]]:
        for each .annot.line in .packing[[.feat.name]][[.pack.line]]:
          draw transcript
```

Key data structures:
- `annot_info[[annotation]][['expanded']]` — GRangesList, one GRanges per
  gene (or merged gene group), transcripts as elements
- `annot_info[[annotation]][['packing']]` — named list, one entry per gene,
  value is list of integer vectors (row → transcript indices)

## Proposed Change

### Strategy: Two-level packing

1. **Level 1 (existing):** Pack transcripts within each gene using
   OrganizeOverlappingIVs. This gives each gene its own internal row
   structure.

2. **Level 2 (new):** Pack genes against each other using their overall
   footprints. Non-overlapping genes can share the same row block.
   Overlapping genes must be in different row blocks (or merged as now).

### Concrete approach

In `OrganizeOverlappingLoci`, after the existing overlapping-gene merging:

1. For each annotation, collect all gene names (both standalone and merged)
2. Get each gene's genomic footprint (start-end of collapsed2 entry)  
3. Get each gene's height in rows (from packing)
4. Run a gene-level packing algorithm:
   - Treat each gene as a rectangle (genomic extent × row height)
   - Pack genes into row-blocks, where genes in the same block don't
     overlap genomically
   - Within a block, each gene's transcripts are drawn at the block's
     vertical offset + the gene's internal row

### Data structure changes

`packing` stays the same structure internally (per-gene row assignments),
but gains a new companion: `gene_row_offsets` — a named integer vector
mapping each gene (or merged group) to its starting row offset.

```r
# Current: each gene starts at row 1 implicitly
annot_info[[annot]][['packing']][['NOP56']] = list(c(1,2), c(3,4), ...)
annot_info[[annot]][['packing']][['SNORD110']] = list(c(1))
annot_info[[annot]][['packing']][['SNORD86']] = list(c(1))

# New: add gene_row_offsets
annot_info[[annot]][['gene_row_offsets']] = c(NOP56=0, SNORD110=10, SNORD86=10, SNORD56=10)
# SNORD110, SNORD86, SNORD56 all start at row 10 (after NOP56's 10 rows)
# and since they don't overlap, they share that row
```

### Affected functions

1. **`OrganizeOverlappingLoci()`** — add gene-level packing after existing
   logic, store result in `gene_row_offsets`
   
2. **`PlotAnnotation()`** — use `gene_row_offsets` to offset `.pack.line`
   when drawing. Minimal change: 
   `.pack.line.offset = gene_row_offsets[.feat.name]`
   then use `.pack.line + .pack.line.offset` for y-position calculation

3. **`RelativeAnnotationHeight()`** — use max(gene_row_offsets + per-gene
   heights) instead of sum of per-gene heights for total row count

4. **`EstimatePlotHeights()`** — same as above, feeds into height calculation

### What stays the same

- `collapsed`, `collapsed2` modes — unchanged
- `OrganizeOverlappingIVs` — unchanged (still used for within-gene packing)
- `AnnotatedFeaturesInRegion` — unchanged
- The `expanded` GRangesList structure — unchanged
- Feature names, text placement — unchanged

### Risk assessment

- **Medium risk:** The change adds a new data slot (`gene_row_offsets`)
  that downstream code must handle. If any consumer doesn't use the offset,
  genes will overlap vertically.
- **Mitigation:** Default `gene_row_offsets` to sequential stacking (current
  behavior) so any code that doesn't know about the offset gets the old
  behavior.
- **Test plan:** Compare expanded-mode plots for NOP56, SNHG12, UBE3A,
  DMD, BRCA1, EGFR before and after. Non-hosted-gene loci (LMO4, MYC)
  should be unaffected.

### Alternative considered

Rewriting the packing from scratch. Rejected because:
- The existing per-gene packing (OrganizeOverlappingIVs) works correctly
- The gene merging for overlapping genes works correctly
- Only the gene-to-row assignment is suboptimal
- Adding a second packing level is lower risk than a rewrite
