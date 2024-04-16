# Architecture Diagram Editor ([WhiskeyJack](AboutTheName.md))[^1]

	Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
	SPDX-License-Identifier: Apache-2.0

This is a start on a tool for editing architecture diagrams, then using those
diagrams and some inferencing to generate and edit an implementation of that
diagram.  Think of it as a top-down IDE.

[![Build](https://github.com/awslabs/whiskey-jack/actions/workflows/maven.yml/badge.svg)](https://github.com/awslabs/whiskey-jack/actions/workflows/maven.yml)

![screenshot](NodeEditorScreenshot.png)

An important thing to understand is the dichotomy between input and output
ports.  (`Port.isInput()`).  For the purpose of graphical rendering, input ports
are drawn on the left side of a node, and output ports are drawn on the right
side of a node.  From a behavioral/algebraic point of view, the outputs are
the clients, the inputs are the servers.  caller/callee, requestor/requestee,
there are lots of variations of the terminology.  The output side of one node
makes requests to the input side of another node.

This is *not* the same as the direction of data flow.  It is just control
flow.  For example, when a request is made to a database, it can be to either
store or retrieve data.

While the diagram editor is what you see, the important part is what's behind the diagram:

Package | Description
:---|:---
nodegraph | *(aws.WhiskeyJack.nodegraph)* The abstract node graph itself and is made up of nodes, ports and arcs.  There is a parallel set of structures that describe metadata for the node (which mostly comes from the product catalog).  In an oddly self-referential twist, the metadata is subclassed from the base object.
infer | Type checking and inferencing.  Finds problems in the interconnection of the graph and highlights them in red.  Then you can either fix the problem manually, or try the "Fix" command to see if the (simple, for now) inferencing engine can find a fix.
code | Translates the diagram into executable code.  Supports multiple code generators for different sub-parts of the graph so they can adapt both to where they are (device, cloud) and how they should be generated (Java, Rust, Cloudfront, Greengrass, ...).
^.toJava | generates java code fragments
^.gradle | generate gradle builds
^.cloudformation | generates AWS CloudFormation templates.
^.greengrass | generates AWS Greengrass recipies.
exl | a **simple** expression language.  Small code snippits can be placed in the graph.  The language is intentionally simplistic so that it has a chance of being translated into multiple underlying languages and of being transformed algebraically.  If you want a real programming language, use one.  `Exl` is intended for interconnection glue and straightforward math.
metadata | represents the "parts catalog" - all the different kinds of nodes that can be used to build the graph.  The parts catalog itself is in src/main/resources/ang/pcats/**.pcat.
QandA | The "question and answer" framework.  Roughly a dynamic dialog box that's context-dependent.
nodeviewerfx | the UI to all of the above, written as a [JavaFX](https://openjfx.io) desktop application.  The UI is in the spirit of the one found in [Blender](https://docs.blender.org/manual/en/2.79/render/blender_render/materials/nodes/introduction.html).  Since the UI is isolated in this one package, it should be possible to re-implement the UI using other technologies.

This is all functional, to a limited extent, but far from truly useful.  Very much a work-in-progress.  For those of you working for Amazon there are a few progress-report videos [here](https://broadcast.amazon.com/channels/58393).

The IDE I use is [Netbeans](https://netbeans.apache.org), so you'll find a few config files from it in the source.  But any other IDE should work too, since this repo uses Maven to automate builds.

The structure of this project is rubbish.  It's all one repo.  It should be
broken up into multiple hierarchic repos. And it should someday be changed to use
Gradle instead of Maven. Mañana

## On Sacred Cows
This implementation treads on two sacred cows:

1. It is a desktop app.  It doesn't need to be, it could become a web app, but it
would suffer.  Desktop apps have extremely fluid interactivity.  And they have
instantaneous access to the whole sematic model during interactions.  Things like
instantaneous error&problem feedback as the mouse moves.

2. It doesn't use genAI in any significant way.  It is the stylish trendy technology
of the day, but it has a drawback that makes using it challenging: it hallucinates,
it is challenging to make it trustworthy.  This is a domain where correctness is
important.

[^1]: <span style="font:italic 20pt serif"> BEWARE This is a Work-In-Progress.
  It does <b>not</b> usefully work.  Careful demos succeed, but it's very much an experiment</span>
