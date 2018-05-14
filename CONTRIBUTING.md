# Contributing to Duckling
We want to make contributing to this project as easy and transparent as
possible.

## Our Development Process
Our changes are first made in Facebook's internal repository, which provides us
with additional lint and static analysis support. Changes are synchronized
periodically to GitHub.

## Pull Requests
We actively welcome your pull requests.

1. Fork the repo and create your branch from `master`.
2. Write tests that describe the expected new behavior from your code changes.
3. Regenerate the classifiers, run the test suite and see your tests failing.
4. Add feature code to make your tests pass.
5. Regenerate the classifiers, run the test suite and see your tests passing.
6. If you haven't already, complete the Contributor License Agreement ("CLA").

## Contributor License Agreement ("CLA")
In order to accept your pull request, we need you to submit a CLA. You only need
to do this once to work on any of Facebook's open source projects.

Complete your CLA here: <https://code.facebook.com/cla>

## Coding Style

### Simple heuristic: Look around
Match the style you see used in the rest of the project. This includes formatting and naming things in code.

### Simple philosophy: Make it 1% better
The code is far from perfectly clean. When the code surrounding your changes is not consistent, go ahead and make it so. However don't clutter your changes with style clean-up; split the pull requests to make the review process easier.

### Guidelines
- 80-char limit.
- Imports: one block for standard modules and other packages, one block for project modules. Qualified imports at the end of each block, alphabetical ordering.

## Issues
We use GitHub issues to track public bugs. Please ensure your description is
clear and has sufficient instructions to be able to reproduce the issue.

Facebook has a [bounty program](https://www.facebook.com/whitehat/) for the safe
disclosure of security bugs. In those cases, please go through the process
outlined on that page and do not file a public issue.

## Code of Conduct

Facebook has adopted a Code of Conduct that we expect project participants to adhere to. Please [read the full text](https://code.facebook.com/pages/876921332402685/open-source-code-of-conduct) so that you can understand what actions will and will not be tolerated.

## License
By contributing to Duckling, you agree that your contributions will be licensed
under the LICENSE file in the root directory of this source tree.
