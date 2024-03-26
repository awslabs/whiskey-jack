/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code;

import aws.WhiskeyJack.QandA.*;
import aws.WhiskeyJack.exl.*;
import aws.WhiskeyJack.nodegraph.*;
import java.io.*;
import java.util.*;
import java.util.function.*;

public class OneDomain implements Closeable {
    private DomainGenerationController generator;
    final Domain domain;
    String targetToken;
    final List<Node> nodes = new ArrayList<>();
    private final OverallCodeGenerationDriver controller;
    private DomainCode code;
    private boolean errorSent;
    OneDomain(OverallCodeGenerationDriver cg, Domain d) {
        controller = cg;
        domain = d;
    }
    private void generator(Consumer<DomainGenerationController> op) {
        var g = generator;
        if(g == null) {
            // TODO This still feels like a bucket of total hacks
            var framework
                = Question.question("runtime", domain).asString().toLowerCase();
            var style = "app";
            var language = Question.question("implang", domain).asString().toLowerCase();
            var path = controller.rootPath.append(domain + "/" + framework + "/" + style + "/" + language);
            generator = g = path.findPlugin(DomainGenerationController.class);
            if(g != null)
                g.setStrategyPath(path);
            else if(!errorSent) {
                controller.error("Can't find code generator for " + framework
                                 + " " + style
                                 + " for the " + language + " language",
                    "Using path: " + path.toString());
                errorSent = true;
            }
        }
        if(g != null) op.accept(g);
    }
    public void prescan() {
        code = new DomainCode(nodes);
        code.optimize();
        code.show();
        generator(g -> g.prescan());
    }
    public void generate() {
//        controller.message(domain + ": " + nodes.size() + " nodes");
        generator(g -> g.generate(code));
    }
    public Collection<Node> getNodes() {
        return nodes;
    }
    @Override
    public void close() {
        generator(g -> g.close());
    }
}
