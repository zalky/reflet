
# Reflet

[![Clojars Project](https://img.shields.io/clojars/v/io.zalky/reflet?labelColor=blue&color=green&style=flat-square&logo=clojure&logoColor=fff)](https://clojars.org/io.zalky/reflet)

<img src="https://i.imgur.com/yWceoD5.jpg" title="zalky" align="right" width="300"/>

**Reflet** is a collection of tools for building Re-frame + React based
web apps. This includes:

1. Entity references and their lifecycle management
2. Efficient, normalized state management
3. Simple but powerful hierarchical FSMs
4. JS and DOM interop utilities
5. Novel API-driven visual debugging of complex apps (don't sleep on
   this!)

Reflet aims to be a natural progression on top of Re-frame to support
complex, data driven requirements. In that sense, it is both easy to
learn, but powerful. You could say it's basically Re-frame++. Its main
design goals are:

1. A la carte feature set: it is not a "framework", so use as much or
   as little of it as you want
2. Bring sanity to an existing Re-frame applications: Reflet
   integrates easily into vanilla Re-frame apps, so a complete
   re-write can be avoided
3. Normalized data model, where entity references connect things
   together: this encourages excellent, pluggable APIs
4. Makes very few assumptions about your application boundaries
5. Performance and stability: Reflet has already been deployed in
   complex, data driven production applications for years
   (e.g. in Bioinformatics, Business analytics... )

<img src="https://i.imgur.com/r1li2r5.jpg" title="zalky" align="center" width="1000"/>

## Who Is This For?

1. Anyone who needs a powerful normalized data model for Re-frame
2. You already have an existing un-normalized Re-frame application,
   and need to take it to the next level
3. You are learning Re-frame, and want to get started right away with
   scalable data models and well-built component APIs

## Installation

At minimum include the following in your `deps.edn`:

```clj
{:deps {io.zalky/reflet {:mvn/version "0.2.0"}}}
```

See additional notes on how to configure the debugger for development.

## Resources

Reflet builds on top of the concepts and design patterns of Re-frame,
and these resources assume a working knowledge of Re-frame basics. If
you are not familiar with Re-frame, [check out that
documentation](https://day8.github.io/re-frame/re-frame/) first, it is
very good. Otherwise:

1. [Overview](#overview)
2. [Configuration](Configuration.md)
3. Feature Documentation
   - [Basics](Basics.md)
   - [Normalized DB](Normalized-DB.md)
   - [Normalized Queries](Normalized-Queries.md)
   - [Finite State Machines](Finite-State-Machines.md)
   - [Mutable State](Mutable-State.md)
   - [Debugging](Debugging.md)
   - [Testing](Testing.md)
6. [Example Client](Example-Client.md)
7. [General Development Tips](General-Development-Tips.md)

## Getting Help

You can either submit an issue here on Github, or alternatively tag me
(`@zalky`) with your question in the `#re-frame` channel on the
[Clojurians](https://clojurians.slack.com) slack.

## License

Reflet is distributed under the terms of the Apache License 2.0.
