# Code Generation Strategy

It's driven by a target string that looks kinda like a pathname:
```
/build/domain/framework/language
```
Where:

tag | description
----|-----
build | the overarching build framework to employ (eg. gradle)
domain | the kind of domain the code is being build for (eg. cloud or device)
framework | the framework (eg. greengrass, freertos, cloudformation)
language | the language to be used for code fragments (eg. java, rust)

Classes that implement some phase of this start with the annotion
```
@GeneratesCodeFor("/gradle/*")
```
Where the string is a glob pattern that matches some part of the target string
and expresses the code's ability to handle that part of the code generation strategy.
Each of these classes is expected to implement some interface that is appropriate to
that part of the strategy.  For example, handlers for the build phase are expected to
implement `OuterBuildController`.

The intent is that this should be pretty open-ended, 
that there could be more layers to the target string.
This is very much the early days of a work-in-progress.

For example, a collection of nodes tagged with `/gradle/device/standalone/java`
will generate a gradle build environment to generate code for a device that
generates a standalone app written in java.  A whole graph will have many
such collections of nodes that are grouped by all of their individual tags.
