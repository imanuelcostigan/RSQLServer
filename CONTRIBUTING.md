## Guidelines

This will provide you with contribution guidelines. You can contribute in two ways:

1. Raising a bug report or making a feature request in an issue
2. Suggesting a change by submitting a pull request

## Issues

* Use the template provided when you create an issue. Follow this template as closely as possible (including the output from `devtools::session_info()`)
* It is ok to submit a stump issue (title only) to ensure something is recorded as long as you subsequently complete it. Issues will not be actioned if they are not complete.


## Pull requests

* You must submit code that follows a [common code style](http://adv-r.had.co.nz/Style.html). PRs that do not follow this style will not be accepted. You may use existing code as inspiration if that is the way you learn.
* Files containing classes should follow the convention `<name>-class.R` where `name` is the class name. Use hyphens to separate words in a file name.
* Use the template that is provided to you when you create a PR.
* New code should be accompanied by [test cases](http://r-pkgs.had.co.nz/tests.html). If it does not, then you must explain why in the PR and create an issue (assigned to yourself) to subsequently submit the test casess.
* New code should be [documented](http://r-pkgs.had.co.nz/man.html) when the object to exported (available to the user). Make sure that you roxygenise the package before submitting the PR if you have added or updated any roxygen blocks by invoking `devtools::document()`.
* Add an item to the [`NEWS` file](http://r-pkgs.had.co.nz/release.html#important-files) that reflects the FIX, CHANGE or NEW functionality. Include any issue numbers that are relevant (e.g. `(#XX)`) where `XX` is the issue number.
* Add your name to the [authors list](http://r-pkgs.had.co.nz/description.html#author) in the `DESCRIPTION` file if you aren't already represented therein
* Only the package maintainer should merge PRs into the master branch. The template PR request includes a ping to the package maintainer.
* Run `devtools::check()` before submitting the PR.


## Workflow

* Adopt the [Github Flow](https://guides.github.com/introduction/flow/) branch based workflow. 
* Branches must almost always be created from master and you will be responsible for resolving any future conflicts with master prior to your PR being submitted. 
* Ideally, branch names should follow the following conventions:
    1. Bug fix branches should be named something like `fix-issue-XX` where `XX` is the corresponding issue number.
    2. Branches that introduce new features should be named something like `feature-new-class-XX` where `XX` is the corresponding issue number
* Commit early and commit often. Other good Git practices can be found [here ](http://r-pkgs.had.co.nz/git.html#commit-best-practices)
* Be nice to other contributors.



