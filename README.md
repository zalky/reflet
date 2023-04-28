
# Reflet

[![Clojars Project](https://img.shields.io/clojars/v/io.zalky/reflet?labelColor=blue&color=green&style=flat-square&logo=clojure&logoColor=fff)](https://clojars.org/io.zalky/reflet)

<img src="https://i.imgur.com/1nfbVFZ.jpg" title="zalky" align="right" width="250"/>

**Reflet** is a set of tools for building
[Re-frame](https://github.com/day8/re-frame) + React based web apps
with graph and non-graph data models.

At the core of Reflet is a single macro, `with-ref`, that generates
references to any kind of thing, and also transparently manages the
lifecycles of those things throughout your application. This leads to
highly extensible components, and excellent APIs.

Besides `with-ref`, Reflet also provides:

1. Performant multi-model DB with graph queries and mutations
2. Data-driven descriptions: a new kind of polymorphic query
3. Simple but powerful hierarchical FSMs
4. JS and DOM interop utilities
5. Novel API-driven visual debugging of complex apps (don't sleep on
   this!)

And more!

Reflet aims to be a natural progression on top of Re-frame to support
complex, data-driven requirements. In that sense, it is both easy to
learn, but powerful. You could say it's sort of like Re-frame++ (or
Fulcro for Re-frame). Its main design goals are:

1. A la carte feature set: it is not a "framework", so use as much or
   as little of it as you want
2. Power up existing Re-frame applications: iterative, minimal
   approach to integration, so big re-writes can be avoided
3. Graph and non-graph data models can be mixed freely with a single
   source of truth: a Clojure map
4. Entity references connect things together: this encourages
   excellent, pluggable APIs
5. Makes very few assumptions about your application boundaries
6. Performance and stability: Reflet has already been deployed in
   complex, data-driven production applications for 4+ years
   (e.g. in Bioinformatics, Business analytics... )

<img src="https://i.imgur.com/6MAwZgS.jpg" title="zalky" align="center" width="1000"/>

## Who Is This For?

1. Anyone who needs a powerful graph data model for Re-frame
2. You already have an existing non-graph Re-frame application, and
   need to take it to the next level
3. You are learning Re-frame, and want to get started right away with
   scalable data models and well-built component APIs

## Installation

At minimum include the following in your `deps.edn`:

```clj
{:deps {io.zalky/reflet {:mvn/version "0.2.0"}}}
```

Or `project.clj`:

```clj
[io.zalky/reflet "0.2.0"]
```

Additionally, React is considered a peer dependency, so you will have
to [ensure that it is
available](https://github.com/zalky/reflet/wiki/Configuration#react). The
same approach you would use to provide React for
[Reagent](https://github.com/reagent-project/reagent) or
[Re-frame](https://github.com/day8/re-frame) will also work for
Reflet.

See [the additional
notes](https://github.com/zalky/reflet/wiki/Configuration#debugger) on
how to configure the debugger for development.

## Resources

Reflet builds on top of the concepts and design patterns of Re-frame,
and these resources assume a working knowledge of Re-frame basics. If
you are not familiar with Re-frame, [check out that
documentation](https://day8.github.io/re-frame/re-frame/) first, it is
very good. Otherwise:

1. [Rationale and Overview](https://github.com/zalky/reflet/wiki#overview)
2. [Quick Start](https://github.com/zalky/reflet/wiki/Quick-Start)
3. [Configuration](https://github.com/zalky/reflet/wiki/Configuration)
4. Features
   - [References and Application Design](https://github.com/zalky/reflet/wiki/References-and-Application-Design)
   - [Multi Model DB](https://github.com/zalky/reflet/wiki/Multi-Model-DB)
   - [Graph Queries](https://github.com/zalky/reflet/wiki/Graph-Queries)
   - [Polymorphic Descriptions](https://github.com/zalky/reflet/wiki/Polymorphic-Descriptions)
   - [Finite State Machines](https://github.com/zalky/reflet/wiki/Finite-State-Machines)
   - [Mutable State](https://github.com/zalky/reflet/wiki/Mutable-State)
   - [Debugging](https://github.com/zalky/reflet/wiki/Debugging)
   - [Testing](https://github.com/zalky/reflet/wiki/Testing)
   - [Advanced Usage](https://github.com/zalky/reflet/wiki/Advanced-Usage)
5. [Example Client](https://github.com/zalky/reflet/wiki/Example-Client)
6. [General Development Tips](https://github.com/zalky/reflet/wiki/General-Development-Tips)

## Getting Help

You can either submit an issue here on Github, or alternatively tag me
(`@zalky`) with your question in the `#re-frame` channel on the
[Clojurians](https://clojurians.slack.com) slack.

## Contribution Acknowledgements

Special thanks to [Inge Solvoll](https://github.com/ingesolvoll).

## License

Reflet is distributed under the terms of the Apache License 2.0.
