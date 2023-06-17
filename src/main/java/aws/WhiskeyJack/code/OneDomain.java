/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.WhiskeyJack.code;

import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import java.io.*;
import java.util.*;


public class OneDomain implements Closeable {
    final DomainGenerationController generator;
    final Domain domain;
    String targetToken;
    final List<Node> nodes = new ArrayList<>();
    private CodeTarget out;
    private final OverallCodeGenerationDriver controller;
    OneDomain(OverallCodeGenerationDriver cg, Domain d, String rootToken) {
        // TODO This is a bucket of total hacks
        controller = cg;
        java.lang.String framework = d == Domain.device ? "greengrass" : d == Domain.cloud ? "apprunner" : d == Domain.browser ? "jquery" : "nothing";
        java.lang.String style = "app";
        java.lang.String language = "java";
        targetToken = rootToken + d + "/" + framework + "/" + style + "/" + language;
        generator = cg.findPlugin(targetToken, DomainGenerationController.class);
        domain = d;
        cg.message(" target token: " + targetToken);
    } // TODO This is a bucket of total hacks
    public void prescan() {
        generator.prescan();
    }
    public void generate() {
        out = generator.makeOutput();
        out.start(domain);
        controller.message("Generating " + out.getPath());
        controller.message(domain + ": " + nodes.size() + " nodes");
        out.comment("Code for domain " + domain);
        generator.generate(nodes, out);
    }
    @Override
    public void close() {
        generator.close();
        Utils.close(out);
    }
}
