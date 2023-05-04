/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.code.toJava;

import aws.jag.DiagramEditor.code.*;
import aws.jag.DiagramEditor.nodegraph.*;
import aws.jag.exl.*;
import java.io.*;
import java.util.*;
import java.util.function.*;
import java.util.regex.*;

@Matches("*/java")
public class JavaGenerator implements DomainGenerationController {
    @Override
    public void generate(List<Node> nodes, CodeTarget out) {
        // TODO aws.jag.DiagramEditor.code.toJava.JavaGenerator.generate Not implemented
        System.out.println("  Whoo! " + this);
        var m = Pattern.compile("^([^.]*)\\..*").matcher(out.getPath().getFileName().toString());
        var cn = m.group(m.matches() ? 1 : 0);
        out.append("package ade.gen;\n\nclass ")
                .append(cn)
                .append(" {\n")
                .indent();
        nodes.forEach(n -> {
            Collection<String> header = new ArrayList<>();
            header.add("Node " + n.getName());
            n.forEachPort((Consumer<Port>) (p -> {
                header.add((p.isInputSide() ? "in  " : "out ") + p.getType() + '\t' + p.getName() + '\t' + p.isConnected());
            }));
            out.ln().comment(header);
            var expr = n.getPort("expr");
            var sti = out.getIndent();
            out.append("void ").append(n.getName()).append("() {\n").indent();
            if(expr != null && !expr.isConnected()) {
                var ocode = expr.getValue();
                if(ocode instanceof String code)
                        try {
                    var parsed = new Parser(new Tokenizer(code)).expression();
                    out.comment("Parsed " + code + " as " + parsed)
                            .append("out = ")
                            .append(parsed)
                            .append(";\n");

                } catch(IOException ex) {
                    out.comment("Error parsing body:", ex.toString());
                    ex.printStackTrace(System.out);
                }
            }
            out.setIndent(sti).append("}\n");
        });
        out.setIndent(0).append("}\n");
    }
    @Override
    public CodeTarget makeOutput(Domain d) {
        return new JavaTarget(d);
    }
}
