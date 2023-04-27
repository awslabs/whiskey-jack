/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.infer;

import aws.jag.DiagramEditor.nodegraph.*;
import aws.jag.exl.*;
import aws.jag.exl.toJava.*;
import java.io.*;
import java.util.*;
import java.util.function.*;

public class CodeGenerator implements Scanner {
    Graph context;
    @Override
    public void Scan(Graph g) {
        context = g;
        if(g.allOK) {
            seperateDomains();
            domains.values().forEach(d -> d.prescan());
            domains.values().forEach(d -> d.generate());
            domains.values().forEach(d -> d.close());
        } else {
            Collection<String> errors = new ArrayList<>();
            g.typeMismatches.forEach((Consumer<Arc>)(a->errors.add(a.oneEnd().getName()+"->"+a.otherEnd().getName()+": "+a.getMessage())));
            errors.addAll(g.otherErrors);
            g.error("Can't run project", "Fix problems first:", errors);
        }
    }
    private final Map<Domain, OneDomain> domains = new HashMap<>();
//    static final Function<Domain,OneDomain> f = d->new OneDomain(d);
//    final Consumer<Node> e = (Node n) ->
//                domains.computeIfAbsent(n.getDomain(), d->new OneDomain(d));
    private void seperateDomains() {
        /* One would think that the cast in the following line is redundent,
         * but errors get flagged without it. */
        context.forEachNode((Consumer<Node>) (n ->
                domains.computeIfAbsent(n.getDomain(), d -> new OneDomain(d)).nodes.add(n)));
    }

    private class OneDomain {
        final Domain domain;
        final List<Node> nodes = new ArrayList<>();
        CodeTarget out;
        OneDomain(Domain d) {
            domain = d;
            out = new JavaTarget(d);
        }
        void prescan() {

        }
        void generate() {
            out.comment("Code for domain " + domain);
            nodes.forEach(n -> {
                Collection<String> header = new ArrayList<>();
                header.add("Node " + n.getName());
                n.forEachPort((Consumer<Port>)(p -> {
                    header.add((p.isInputSide() ? "in  " : "out ")+p.getType()+'\t'+p.getName()+'\t'+p.isConnected());
                }));
                out.nl().comment(header);
                var expr = n.getPort("expr");
                if(expr!=null && !expr.isConnected()) {
                    var ocode = expr.getValue();
                    if(ocode instanceof String code) {
                        try {
                            var parsed = new Parser(new Tokenizer(code)).expression();
                            out.comment("Parsed "+code+" as " + parsed);
                            out.append("public boolean test() {\n\treturn ")
                                    .append(parsed)
                                    .append(";\n}\n");
                        } catch(IOException ex) {
                            out.comment("Error parsing body:", ex.toString());
                            ex.printStackTrace(System.out);
                        }
                    }
                }
            });
        }
        void close() {
            out.close();
        }
    }
}
