((magit-blame
  ("-w"))
 (magit-commit nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  (("--" "./"))
  ("--author=Graham Clark <gclark@extremenetworks.com>"
   ("--" "./")))
 (magit-push nil)
 (magit:--author "Graham Clark <gclark@extremenetworks.com>"))
