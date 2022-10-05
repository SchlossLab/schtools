## Test environments

- local macOS install; R 4.2.1
- win-builder; R release
- github-actions macOS-latest; R release
- github-actions ubuntu-latest; R release, devel, and oldrel
- github-actions windows-latest; R release

## R CMD check results

0 errors | 0 warnings | 1 note

Found the following (possibly) invalid URLs:
  URL: https://anaconda.org/conda-forge/r-schtools
    From: README.md
    Status: 400
    Message: Bad Request
  URL: https://doi.org/10.1128/mbio.03161-21
    From: inst/doc/introduction.html
          NEWS.md
    Status: 503
    Message: Service Unavailable
    
These URLs work in my browser.

## revdepcheck results

No reverse dependencies.
