## Test environments
* Ubuntu 16.04, R 3.4.4 (local)
* Ubuntu 14.04, R devel-r75089 (Travis-CI)
* Ubuntu 14.04, R 3.5.0 (Travis-CI)
* Ubuntu 14.04, R 3.4.4 (Travis-CI)
* OS X 10.12, R 3.5.0 (Travis-CI)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

## Submission comments

2018-09-08

`devtools::build_win()` reports a warning:

```
Warning: unable to re-encode 'make_epiflows.R' line 198
```

It appears to be harmless (see e.g. http://r.789695.n4.nabble.com/Error-on-Windows-build-quot-unable-to-re-encode-quot-td1570626.html )
