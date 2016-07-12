# Setting up Travis

I have never used [Travis] before and right now don't really care about
continuous integration here since this is a toy project, and nobody should use
this in production.

But what I *do* want to have is automatic building of documentation into the
GH-Page of the project. This is apparently state-of-the-art regarding rust
projects and I personally really like that approach. After some search I found
an [entry on Reddit][1] pointing to [travis-cargo].

Looks like this is exactly what I want, and if I have to do the whole Travis
setup process for this to work, so be it. The repository linked to
comprehensive tutorial going through the process step by step. Since the
documentation is build and published by Travis an access token has to be
generated and stored encrypted inside the `.travis.yml` file. The [Travis Gem]
can do the encryption step for you.

Setting up everything was quite fast, even for the first time doing it.

[1]: <https://www.reddit.com/r/rust/comments/3e1xgy>
[Travis]: <https://travis-ci.org/>
[Travis Gem]: <https://github.com/travis-ci/travis.rb>
[travis-cargo]: <https://github.com/huonw/travis-cargo>
