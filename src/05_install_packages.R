
# The aim of this code is to install the required packages
# with the version specified

#install.packages("data.table", type = "source")

# to install older versions of packages, first do remove.packages()

devtools::install_git(
  "https://gitlab.com/stapm/alc.tools.git",
  credentials = git2r::cred_user_pass("dosgillespie", getPass::getPass()),
  ref = "0.2.0",
  build_vignettes = TRUE
)

devtools::install_git(
  "https://gitlab.com/stapm/hseclean.git",
  credentials = git2r::cred_user_pass("dosgillespie", getPass::getPass()),
  ref = "1.3.0",
  build_vignettes = TRUE
)

devtools::install_git(
  "https://gitlab.com/stapm/tobalcepi.git",
  credentials = git2r::cred_user_pass("dosgillespie", getPass::getPass()),
  ref = "1.1.0",
  build_vignettes = TRUE
)

devtools::install_git(
  "https://gitlab.com/stapm/mort.tools.git",
  credentials = git2r::cred_user_pass("dosgillespie", getPass::getPass()),
  ref = "1.0.1",
  build_vignettes = TRUE
)

devtools::install_git(
  "https://gitlab.com/stapm/stapmr.git",
  credentials = git2r::cred_user_pass("dosgillespie", getPass::getPass()),
  ref = "0.5.2",
  build_vignettes = TRUE
)
