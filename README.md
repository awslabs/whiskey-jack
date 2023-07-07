# Architecture Diagram Editor ([WhiskeyJack](AboutTheName.md))

	Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
	SPDX-License-Identifier: Apache-2.0

This is a start on a tool for editing architecture diagrams, then using those
diagrams and some inferencing to generate and edit an implementation of that
diagram.  Think of it as a top-down IDE.

[![Build](https://github.com/awslabs/whiskey-jack/actions/workflows/maven.yml/badge.svg)](https://github.com/awslabs/whiskey-jack/actions/workflows/maven.yml)

![screenshot](NodeEditorScreenshot.png)

While the diagram editor is what you see, the important part is what's behind the diagram:

Package | Description
:---|:---
nodegraph | *(aws.WhiskeyJack.nodegraph)* The abstract node graph itself and is made up of nodes, ports and arcs.  There is a parallel set of structures that describe metadata for the node (which mostly comes from the product catalog).  In an oddly self-referential twist, the metadata is subclassed from the base object.
infer | Type checking and inferencing.  Finds problems in the interconnection of the graph and highlights them in red.  Then you can either fix the problem manually, or try the "Fix" command to see if the (simple, for now) inferencing engine can find a fix.
code | Translates the diagram into executable code.  Supports multiple code generators for different sub-parts of the graph so they can adapt both to where they are (device, cloud) and how they should be generated (Java, Rust, Cloudfront, Greengrass, ...).
exl | a *simple* expression language.  Small code snippits can be placed in the graph.  The language is intentionally simplistic so that it has a chance of being translated into multiple underlying languages.  If you want a real programming language, use one.  `Exl` is intended for interconnection glue and straightforward math.
metadata | represents the "parts catalog" - all the different kinds of nodes that can be used to build the graph.  The parts catalog itself is in src/main/resources/ang/pcats/**.pcat.
QandA | The "question and answer" framework.  Roughly a dynamic dialog box that's context-dependent.
nodeviewerfx | the UI to all of the above, written as a [JavaFX](https://openjfx.io) desktop application.  The UI is in the spirit of the one found in [Blender](https://docs.blender.org/manual/en/2.79/render/blender_render/materials/nodes/introduction.html).  Since the UI is isolated in this one package, it should be possible to re-implement the UI using other technologies.

This is all functional, to a limited extent, but far from truly useful.  Very much a work-in-progress.  For those of you working for Amazon there are a few progress-report videos [here](https://broadcast.amazon.com/channels/58393).

The IDE I use is [Netbeans](https://netbeans.apache.org), so you'll find a few config files from it in the source.  But any other IDE should work too, since this repo uses Maven to automate builds.

The structure of this project is rubbish.  It's all one repo.  It should be
broken up into multiple hierarchic repos. And it should someday be changed to use
Gradle instead of Maven. Mañana