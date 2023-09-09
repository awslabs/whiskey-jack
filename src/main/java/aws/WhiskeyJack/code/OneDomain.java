/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code;

import aws.WhiskeyJack.QandA.*;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import java.io.*;
import java.util.*;

public class OneDomain implements Closeable {
    private DomainGenerationController generator;
    final Domain domain;
    String targetToken;
    final List<Node> nodes = new ArrayList<>();
    private CodeTarget out;
    private final OverallCodeGenerationDriver controller;
    OneDomain(OverallCodeGenerationDriver cg, Domain d) {
        controller = cg;
        domain = d;
        cg.message(" target token: " + targetToken);
    }
    private DomainGenerationController generator() {
        var g = generator;
        if(g == null) {
        // TODO This still feels like a bucket of total hacks
            java.lang.String framework = domain == Domain.device
                    ? Question.question("devrun").asString().toLowerCase()
                    : domain == Domain.cloud ? Question.question("cloudrun").asString().toLowerCase()
                            : domain == Domain.browser ? "jquery" : "nothing";
            java.lang.String style = "app";
            java.lang.String language = "java";
            var path = controller.rootPath.append(domain + "/" + framework + "/" + style + "/" + language);
            generator = g = path.findPlugin(DomainGenerationController.class);
            g.setStrategyPath(path);

        }
        return g;
    }
    public void prescan() {
        generator().prescan();
    }
    public void generate() {
        out = generator().makeOutput();
        out.start(domain);
        controller.message("Generating " + out.getPath());
        controller.message(domain + ": " + nodes.size() + " nodes");
        out.comment("Code for domain " + domain);
        generator().generate(nodes, out);
    }
    @Override
    public void close() {
        generator().close();
        Utils.close(out);
    }
}
