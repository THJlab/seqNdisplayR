# Importing an IGV Session into seqNdisplayR

`seqNdisplayR::IGV2Session()` converts an IGV `.xml` session into a seqNdisplayR
session by inferring the biological structure from track names and `autoscaleGroup`
attributes. The conversion handles a wide range of IGV sessions, but a session
that follows a few naming conventions converts much more cleanly. This page lists
those conventions, what the importer does internally, and how to handle the
"partial Excel" workflow when the heuristic can't fully infer the structure.

---

## Choosing a grouping strategy

`IGV2Session()` offers four strategies for `group_by`, ordered from
most-automatic to most-manual:

1. **`common_prefix`** (default) — greedy boundary-aligned prefix clustering
   on track names. Works for any session with consistent naming
   conventions; no IGV-side setup required.
2. **`autoscalegroups`** — uses the `autoscaleGroup` attribute on each
   track. Best when the session was deliberately annotated with biological
   autoscaleGroups (see [autoscaleGroup convention](#optional-using-autoscalegroup)
   below).
3. **`directory`** — every parent directory of the bigWig file is a dataset.
   Useful when files are organised by experiment in folders.
4. **`none`** — every track becomes its own dataset. Verbose fallback when
   nothing else fits.

The Shiny app's "IGV grouping strategy" dropdown defaults to `common_prefix`.
If your session doesn't convert cleanly with the default, try the others
in order.

---

## Recommended IGV session conventions

These conventions help **all** grouping strategies, but especially the
default `common_prefix`. None are mandatory; each one trades a small bit
of upfront effort for fewer `<FILL_ME:...>` placeholders to fix later.

### 1. Give tracks meaningful display names

The importer reads the `name=` attribute on each `<Track>` element. Clean,
human-readable names work much better than bare bigWig filenames:

| Track name                          | Result                                  |
|-------------------------------------|-----------------------------------------|
| `3-seq siCTRL (+)`                  | Dataset `3-seq`, subgroup `siCTRL`      |
| `FNBP4_nodTAG_rep1_plus.bw`         | Dataset `FNBP4`, subgroup `nodTAG`      |
| `track1`                            | Heuristic gives up → `<FILL_ME:...>`    |

If you've already loaded your tracks in IGV with default (filename-based)
names, double-click each track and edit the name before saving the session.

### 2. Use consistent strand suffixes

The importer detects strand from the **filename** by looking for `_plus` /
`_minus`. Keep this consistent between plus and minus partners:

| Good (paired correctly)              | Bad (won't pair)                              |
|--------------------------------------|-----------------------------------------------|
| `foo_plus.bw` / `foo_minus.bw`       | `foo_plus.bw` / `foo_minus_posVal.bw`         |
| `xxx_rep1_plus.bw` / `xxx_rep1_minus.bw` | `xxx_plus.bw` / `xxx_minus_corrected.bw`  |

When pairing fails, the minus track ends up in a separate subgroup from its
plus partner, doubling the apparent track count.

### 3. Pattern your filenames as `<dataset>_<condition>_rep<N>_<strand>.bw`

When track display names match the filenames (no manual renaming), the
importer's two-pass heuristic recovers a clean hierarchy from this pattern:

```
FNBP4_nodTAG_rep1_plus.bw   → dataset=FNBP4, subgroup_1=nodTAG, rep=1, strand=+
FNBP4_nodTAG_rep2_plus.bw   → dataset=FNBP4, subgroup_1=nodTAG, rep=2, strand=+
FNBP4_4HdTAG_rep1_plus.bw   → dataset=FNBP4, subgroup_1=4HdTAG, rep=1, strand=+
...
```

Mixing patterns within one session (some tracks named `dataset_cond_rep_strand`
and others named `dataset-cond.rep.strand`) forces the heuristic into per-track
fallbacks.

### 4. Avoid mid-name dots

`.` is a hierarchy separator in seqNdisplayR sample names. The importer strips
the `.bw` / `.bigWig` extension automatically, but other dots in the name (e.g.
`GSE_X.subset.bw` → `GSE_X.subset`) get converted to `_`. This is safe but ugly.

Prefer underscores: `GSE_X_subset.bw`.

### 5. Use URLs (not local paths) for portability

Tracks loaded from `http://`, `https://` or `ftp://` work for anyone with
network access. Tracks loaded from local paths like
`/Users/yourname/Desktop/.../foo.bed` only work on your machine.

The "Check File" diagnostic in the Shiny app will WARN you about local
annotation paths so you know they need to be replaced before sharing the
session with collaborators.

### 6. Avoid duplicate URLs

A single bigWig file referenced under two different track names confuses the
strand-pairing step. Use one Resource per file.

### 7. (Optional) Using `autoscaleGroup` — split + and − into sister groups

Only relevant if you want to use `group_by = "autoscalegroups"`. Tracks
sharing an `autoscaleGroup` value become **one dataset** in seqNdisplayR.
Set the same `autoscaleGroup` value on all replicates of the same sample,
but **keep + and − strands in SEPARATE autoscaleGroups**.

Why split the strands? In IGV, `autoscaleGroup` ties the value range of the
tracks together. Plus and minus strands typically have very different value
distributions, so autoscaling them as one group loses meaning. Best practice
is sister groups:

| Tracks                                          | autoscaleGroup |
|-------------------------------------------------|----------------|
| 3-seq siCTRL replicates, **plus** strand        | `"1"`          |
| 3-seq siCTRL replicates, **minus** strand       | `"2"`          |
| 3-seq siEXOSC3 replicates, **plus** strand      | `"3"`          |
| 3-seq siEXOSC3 replicates, **minus** strand     | `"4"`          |

The importer bridges sister groups automatically: each minus track adopts its
plus partner's `autoscaleGroup` (matched by filename via `_plus` / `_minus`)
before dataset names are inferred, so the two halves are processed as one
biological group.

In IGV: select the relevant tracks of one strand → right-click → *Group autoscale*.
Repeat for the other strand.

---

## How `IGV2Session()` works

1. Parses `<Track>` and `<Resource>` elements from the XML.
2. Identifies strand from the **filename** via the `strand_regex` parameter
   (default `_plus` / `_minus`).
3. Groups tracks into datasets via the `group_by` strategy:
   - `common_prefix` (**default**): greedy boundary-aligned prefix clustering on names.
   - `autoscalegroups`: tracks sharing `autoscaleGroup` become one dataset. Before grouping, each minus track adopts its plus partner's `autoscaleGroup` (sister groups → one biological group).
   - `directory`: parent directory becomes the dataset.
   - `none`: every track is its own dataset.
4. Strips `_plus` / `_minus` strand suffix, `.bw` / `.bigWig` extension, and
   trailing/leading separators from names.
5. For each group, extracts the longest **boundary-aligned** shared prefix
   as the dataset name. The residue becomes `subgroup_1`.
6. Pairs minus tracks with their plus partners (dataset, subgroup_1, color
   inherited).
7. Runs a **second-level pass**: within each dataset, re-clusters by the
   `subgroup_1` values to find subgroupings (e.g. `nodTAG`/`4HdTAG` under
   `FNBP4`). Replicates of the same sample collapse into rows that share
   `(dataset, subgroup_1)`.
8. Unifies colors within each cluster.

When step 5 or step 7 can't find a meaningful prefix, the dataset name is
set to a placeholder like:

```
<FILL_ME: synthetic dataset (autoscaleGroup=3); please rename>
```

These placeholders are surfaced to the user via the partial-Excel workflow.

---

## The partial Excel workflow

When `IGV2Session()` leaves `<FILL_ME:...>` placeholders or the session has
no annotations:

1. **Save Settings** in the Shiny app — this writes an Excel file via
   `Session2xlsx()`. If placeholders exist, an extra **README sheet** is
   prepended explaining what to fix and where.
2. **Open the Excel** and edit the flagged cells:
   - Replace `<FILL_ME:...>` dataset names with meaningful labels.
   - Populate the `ANNOTATIONS` sheet with BED files if you want annotations.
3. **Re-upload** the edited Excel file in the Shiny app.
4. **Click "Check File"** — `CheckSampleFile()` will WARN on any remaining
   `<FILL_ME:...>` cells so you know nothing was missed.

---

## IGV session features that are NOT (yet) imported

These are silently ignored by `IGV2Session()`; they need to be set in the
seqNdisplayR session afterwards if you want them:

- **IGV `locus` attribute** — written on export but ignored on import.
- **`DataRange` (per-track min/max)** — IGV uses 0–50 by default; seqNdisplayR
  computes scales separately or via `force_scale`.
- **`MergedTracks` wrapper names** — the user-supplied label on a MergedTracks
  block is dropped; its children are imported as individual tracks.
- **Multi-panel sessions** — flattened to a single `DataPanel` + `FeaturePanel`.
- **Batch information** — IGV has no equivalent; `batch` is always `NA` on
  import. Set per-row in the Excel if needed.

---

## See also

- `?IGV2Session` — the import function (`group_by`, `strand_regex`, etc.)
- `?CheckSampleFile` — diagnostic for both `.xml` and `.xlsx` files
- `?Session2xlsx` — export to Excel template
- `?Session2IGV` — the reverse direction (sNdR session → IGV `.xml`)
