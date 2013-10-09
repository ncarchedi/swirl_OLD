This is the official repository for the swirl (Statistics with Interactive R Learning) R package.

See our website for more information: http://ncarchedi.github.io/swirl/

If you wish to install and run the package on your computer, perform the following commands from your R console:

```
install.packages("devtools")
library(devtools)
install_github(repo="swirl", username="ncarchedi")
library(swirl)
swirl()
```

If you are experiencing a problem with the software, please check the "Issues" page of our GitHub repository (where we maintain our code) to see if someone else has already posted the same issue:

https://github.com/ncarchedi/swirl/issues

If no one has done so, then please create a "New Issue" and include the following items in your post:

- Name of the course/module within which you are experiencing the problem
- Brief description of the problem
- Your operating system and R version number (found by typing ```R.Version()$version.string``` in the console)
- Any screenshots or output that might help us visualize the problem