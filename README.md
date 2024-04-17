# sidewindr
an R package meant to be a tidyverse/dplyr-esque, yet angled at being more friendly towards base R.

# Installation
to install `sidewindr`, please download the source binary by downloading the zip from the `<>Code` button near the top of the GitHub page or by cloning the repository. Once you have the code on your personal machine, copy the path to the sidewindr_x.x.x.tar.gz file. In R, run `install.packages(path)` where `path` is the path to the .tar.gz file you downloaded from the GitHub. This will let R compile the package using the provided binaries. Once this has been done, you should be able to attach sidewindr like any other package using `library('sidewindr')`.

Note: The filepath to the package needs to use FORWARD SLASHES `/`, not back slashes `\`.

# Dependencies
requires rlang (version >= 1.0)
