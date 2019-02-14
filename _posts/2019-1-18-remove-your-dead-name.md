---
layout:   post
title:    "Remove Your Dead Name From Git Commits"
date:     2019-1-18
excerpt:  "How to overwrite your git commits with an updated name!"
comments: false
tag:
- git
---

For production environments or other places where you might need to
preserve your git history, consider adding a .mailmap file to the top
level of your directory instead. You can read more about how to use
.mailmap files [here](https://git-scm.com/docs/git-check-mailmap).
Thanks to [Josh Triplett](https://twitter.com/josh_triplett) for
suggesting this solution.

If you are looking for overwrite your git history on smaller,
personal projects, you can
```
$ git filter-branch --commit-filter '
GIT_AUTHOR_NAME="[your name]"
GIT_COMMITTER_NAME="[your name]"
git commit-tree "$@"' -f
```

```
$ git push --force-with-lease
```

For a project that has multiple contributors you can something like the following:
```
git filter-branch --commit-filter '
if [ "$GIT_AUTHOR_NAME" = "[previous name]" ];
then
        GIT_AUTHOR_NAME="[new name]";
        GIT_COMMITTER_NAME="[new name]";
        git commit-tree "$@";
else
        git commit-tree "$@";
fi' HEAD
```

The previous example is slightly modified from one of the
examples from the documentation on the
[filter-branch command](https://git-scm.com/docs/git-filter-branch).

Cheers to having only your chosen name in your git commits! 🎉

This blog post is a more searchable archive of [this twitter
thread](https://twitter.com/DebugSteven/status/1082402318539243520).
