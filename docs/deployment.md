# Deploying the seqNdisplayR Shiny app

This document covers three deployment paths for the seqNdisplayR Shiny app
and explains the trade-offs. The provided [Dockerfile](../Dockerfile) is the
common artifact for paths A and C; path B is not feasible without a
material refactor (see below).

---

## TL;DR — local Docker run

The fastest way to verify the deployment story end-to-end is on your own
machine:

```bash
# from the package root
docker build -t seqndisplayr:2.0.0 .
docker run --rm -p 3838:3838 seqndisplayr:2.0.0
# open http://localhost:3838 in a browser
```

The first build takes 15-30 minutes (Bioconductor and its dependencies are
the slow step). Subsequent builds reuse layers and finish in seconds when
only the R source changed, because the Dockerfile installs Bioconductor /
CRAN / GitHub deps in earlier layers and the package source in the last
layer.

Mount a directory of bigwig / annotation files into the container so the
app can read them by absolute path:

```bash
docker run --rm -p 3838:3838 \
  -v /lab/storage/bigwigs:/data/bigwigs:ro \
  -v /lab/storage/annotations:/data/annotations:ro \
  seqndisplayr:2.0.0
```

Excel / IGV session files uploaded through the app should then reference
files via the in-container paths (`/data/bigwigs/...`,
`/data/annotations/...`).

---

## Path A — lab-internal Shiny Server / Docker host

**Best for:** an internal audience on the lab VPN or a known IP allowlist.
Bigwig and annotation files live on a server filesystem the app can mount
directly; sample-sheet uploads point at those paths.

**Setup**

1. Provision a Linux host (Ubuntu 22.04 / Debian 12 / RHEL 9 all fine).
   The c2 ILP solver needs GLPK ≥ 4.57 (the OS packages are recent enough).
2. Install Docker.
3. Mount the bigwig / annotation storage volume read-only.
4. Run the image as above, optionally behind nginx for TLS + basic-auth.

**nginx in front of Docker**

```nginx
server {
    listen 443 ssl;
    server_name seqndisplayr.lab.example.org;

    # TLS via Let's Encrypt / lab CA
    ssl_certificate     /etc/ssl/seqndisplayr/fullchain.pem;
    ssl_certificate_key /etc/ssl/seqndisplayr/privkey.pem;

    # Basic auth -- generate with `htpasswd -c /etc/nginx/.htpasswd user`
    auth_basic           "seqNdisplayR";
    auth_basic_user_file /etc/nginx/.htpasswd;

    location / {
        proxy_pass http://127.0.0.1:3838;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
        proxy_read_timeout 1800s;   # long-running plot/render
    }
}
```

**Auth caveats**

- Shiny Server Open Source has no built-in user management; lean on
  nginx basic-auth, a VPN, or an IP allowlist.
- Shiny is single-user-per-process by default. Two users hitting the same
  Shiny process share an R session unless you run multiple instances
  behind a load balancer — at which point path C (ShinyProxy) is simpler.
- Bigwig files behind HTTP basic-auth: the user must embed credentials in
  the URL (`https://user:password@host/path.bw`). The "Check File" step in
  the app warns about this when a URL is unreachable.

**Memory**

- The in-process bigwig and annotation caches grow without a hard cap;
  call `bigwig_cache_size()` / `annotation_cache_size()` from R to inspect
  current size, or `clear_bigwig_cache()` / `clear_annotation_cache()` to
  free memory. For a server with many users hitting many regions, restart
  the container periodically or run it under ShinyProxy (path C) so each
  session gets a fresh process.

---

## Path B — shinyapps.io / Posit Connect

**Not recommended for v2.0.0.** Blocked by the GLPK system dependency.

`ompr.roi` + `ROI.plugin.glpk` require `libglpk` (a C library) to be
installable at build time. shinyapps.io builds run in a managed container
where system libraries beyond a fixed allow-list cannot be added; Posit
Connect on the public hosted plan has the same limitation. Bringing the
app to either platform requires one of:

1. Swap the LP backend to `ROI.plugin.lpsolve` (CRAN-only, no system
   dep). The lpsolve solver is slower than GLPK and has different
   tie-break behavior — a non-trivial verification effort.
2. Use a pure-R LP solver (`Rglpk` won't help — same C dep).
3. Pre-render the c2 ILP results server-side and ship a static viewer
   (defeats the point of an interactive app).

Tracked in [docs/TODO.md](TODO.md) as a v2.1+ stretch goal.

---

## Path C — Docker + ShinyProxy (per-user containers)

**Best for:** external users, multi-tenant access, OIDC/SAML auth, or
when you want each session in its own fresh R process.

**Setup**

1. Build the same Docker image (see TL;DR).
2. Install [ShinyProxy](https://www.shinyproxy.io) on the host.
3. Reference the image in `application.yml`:

```yaml
proxy:
  title: seqNdisplayR
  port: 8080
  authentication: openid          # or simple, ldap, saml, ...
  openid:
    auth-url: https://idp.example.org/.well-known/openid-configuration
    client-id: seqndisplayr
    client-secret: ${OIDC_SECRET}
  specs:
    - id: seqndisplayr
      display-name: seqNdisplayR
      container-image: seqndisplayr:2.0.0
      container-cmd: ["R", "-q", "-e",
        "seqNdisplayR::run_seqNdisplayR_app(host='0.0.0.0', port=3838L, launch.browser=FALSE)"]
      port: 3838
      container-volumes:
        - "/lab/storage/bigwigs:/data/bigwigs:ro"
        - "/lab/storage/annotations:/data/annotations:ro"
      container-memory-limit: 4g
      container-cpu-limit: 2
```

4. Front with nginx for TLS termination.

**Trade-offs**

- Each user gets a clean container — no cross-session state, no shared
  caches between users.
- Memory cost scales linearly with concurrent users; size the host
  accordingly.
- Cold-start cost: starting a container per session adds 3-10 s of
  latency before the first plot. Acceptable for most lab workflows;
  mitigate with `proxy.container-pre-initialization` if needed.

---

## File upload support

The app today accepts Excel / IGV / XML uploads but does **not** accept
user-uploaded bigwig or BED files; it expects URL or filesystem paths it
can resolve. If you need true ad-hoc upload of large binary files:

- Add a `fileInput` wired to a per-session temp directory.
- Cap upload size via `options(shiny.maxRequestSize = ...)` in
  `inst/shiny/seqNdisplayR_app.R`.
- Set per-user quotas at the orchestrator layer (ShinyProxy supports
  `container-volumes` with quota-managed tmpfs).

This is a v2.1+ enhancement; v2.0.0 ships read-only.

---

## Verification checklist

Before declaring a deployment ready:

- [ ] `docker build` completes cleanly from a fresh clone
- [ ] `docker run` starts the app and the home page renders in a browser
- [ ] Upload one of the bundled example xlsx files
  (`inst/extdata/sNdR_sample_example_simple.xlsx`) and render a plot
- [ ] Upload an IGV `.xml` and verify the auto-CheckFile diagnostic
  surfaces in the UI
- [ ] Verify the "Check File" warning message mentions password-protected
  URLs when an unreachable HTTPS path is included in the sample sheet
- [ ] Plot one dense region (NOP56 / chr11:+:93700000:93740000) to
  confirm the c2 ILP path runs under GLPK without errors
- [ ] Hit the app from a second browser tab to confirm a fresh session
  starts cleanly
