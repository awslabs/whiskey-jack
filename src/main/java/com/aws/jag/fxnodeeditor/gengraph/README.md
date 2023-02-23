# *Gen*eric *Graph*
### Classes

Class | Meaning
---- | -------
Graph | the context for a graph -- it mostly contains a map from unique IDs to Nodes.
Node | A node in the graph.  Every node has an associated Graph and a metadata descriptor.  Nodes exist within a Domain
Port | Nodes contain _ports_ which are the origin of an arc connecting one node to another.  A port has a Type.  A port may be the endpoint of many arcs.
Type | a description of the data that flows across an arc.  An image?  A temperature reading?  An event?  
Arc | A connection from one Node to another.  Arc objects point from one Node's Port to another Node's Port.
MetaNode | Roughly like a class declaration in a programming language.  Most often shows up in a product catalog (not a great term).  Contains a set of MetaPorts to describe the Node's connectivity.
MetaPort | The descriptor for a connection from a Node.  It has a name and data Type.


### Kinds of graphs

Kind | Use
---- | ---
Model | The graph being edited
View | Twin of a Model that contains all the goo necessary to display the graph.
Expanded | A model that has been through all the compile/inferencing steps and has derived nodes inserted and is annotated with messages.
