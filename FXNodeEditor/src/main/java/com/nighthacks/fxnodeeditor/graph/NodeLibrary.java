/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import com.nighthacks.fxnodeeditor.*;
import com.nighthacks.fxnodeeditor.util.*;
import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.function.*;
import java.util.regex.*;
import javafx.event.*;

public class NodeLibrary {
    public final Map<String, MNode> flatmap = new HashMap<>();
    public final MNode root = new MNode(null, "root") {
        @Override
        public boolean isRoot() {
            return true;
        }
    };
    private NodeEditorController nec;
    private Path rootPath;
    public MNode add(String group, String name) {
        var n = root.createIfAbsent(group).createIfAbsent(name);
        flatmap.put(name, n);
        return n;
    }
    public void forAll(Consumer<MNode> f) {
        System.out.println("\nFA " + root.fullname());
        root.forEach(f);
    }
    public MNode createIfAbsent(String name) {
        var names = joiners.split(name);
        if(names.length == 1)
            return flatmap.get(names[0]);
        MNode v = root;
        for(String part: names)
            v = v.createIfAbsent(part);
        return v;
    }
    static String tname(Object o) {
        return o.getClass().getCanonicalName();
    }
    public void exportAction(ActionEvent t) {
        try( CommitableWriter out = CommitableWriter.abandonOnClose(rootPath.resolve("total.mn"))) {
            NodeEditorController.fileio.writeValue(out, Collectable.asObject(root));
            out.commit();
        } catch(IOException ioe) {
            Dlg.error("Can't save file", ioe);
        }
    }
    public void initialize(NodeEditorController n) {
        nec = n;
        rootPath = Config.scanConfig("mn", (a, b) -> System.out.println("MN " + a + " " + b));

        // stopgap
        add("input", "constant").out("v", 0);
        add("input", "slider").out("v", 0);
        add("input", "sin").in("freq", 0.5).in("amp", 1).out("v", 0);
        add("output", "chart").in("v", 0);
        add("output", "log").in("v", 0).in("log", "node.log");
        add("filter", "clamp").in("v", 0).in("min", 0).in("max", 100).out("v", 0);
        add("filter", "expmean").in("v", 0).in("window", 10).out("v", 0);
        add("filter", "boxmean").in("v", 0).in("window", 10).out("v", 0);
        add("filter", "ratelimit").in("v", 0).in("limit", 2).out("v", 0);
        add("math", "sum").in("a", 0).in("b", 0).out("v", 0);
        add("math", "diff").in("a", 0).in("b", 0).out("v", 0);
        add("math", "eval").in("a", 0).in("b", 0).in("c", 0).in("expr", "a+b").out("v", 0);
        root.dump(0);
    }
    private static final Pattern joiners = Pattern.compile(" *[.,:/] *");
}
