((magit-blame
  ("-w"))
 (magit-commit nil)
 (magit-diff
  ("--no-ext-diff" "--stat"))
 (magit-gitignore nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  (("--" "./"))
  ("--author=Graham Clark <gclark@extremenetworks.com>"
   ("--" "./")))
 (magit-push nil)
 (magit-stash
  ("--all"))
 (magit:--author "Graham Clark <gclark@extremenetworks.com>"))
