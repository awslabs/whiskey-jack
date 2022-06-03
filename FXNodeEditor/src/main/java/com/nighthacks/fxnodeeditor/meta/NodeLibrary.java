/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.meta;

import com.nighthacks.fxnodeeditor.graph.*;
import com.nighthacks.fxnodeeditor.util.*;
import static com.nighthacks.fxnodeeditor.util.Utils.*;
import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.function.*;
import java.util.regex.*;
import javafx.event.*;

/**
 * A library of meta nodes.  The "product catalog"
 */
public class NodeLibrary {
    public final MetaNode root = new MetaNode(null, "root") {
        @Override
        public boolean isRoot() {
            return true;
        }
    };
    private Path rootPath;
    public void forAll(Consumer<MetaNode> f) {
        root.forAllLeaves(f);
    }
    public MetaNode createIfAbsent(String name) {
        if(isEmpty(name) || "root".equals(name))
            return root;
        var names = joiners.split(name);
        var v = root;
        for(var part: names)
            v = v.createIfAbsent(part);
        return v;
    }
    static String tname(Object o) {
        return o.getClass().getCanonicalName();
    }
    public void exportAction(ActionEvent t) {
        saveAllDirty();
//        saveAllAs(rootPath.resolve("total.mn"));
    }
    public void saveAllAs(Path p) {
        try( var out = CommitableWriter.abandonOnClose(p)) {
            NodeEditorController.fileio.writeValue(out, Collectable.asObject(root));
            out.commit();
        } catch(IOException ioe) {
            Dlg.error("Can't save file", ioe);
        }
    }
    public void saveAllDirty() {
        System.out.println("Save All Dirty");
        var l = new ArrayList<String>();
        root.writeDirty(l);
        if(l.isEmpty()) Dlg.note("No modified product catalogs");
        else Dlg.note("Wrote:"+l);
    }
    public void load(String tag, Path fn) {
        try {
//            System.out.println("load " + tag + " " + fn);
            CommitableReader.of(fn).read(in -> {
                var fromFile = fn;
                var v = NodeEditorController.fileio.readValue(in, Object.class);
//                System.out.println(Utils.deepToString(v, 80));
                switch(v) {
                    case Map m -> {
                        var rootName = Coerce.get(m, "name", "");
                        var node = (!rootName.isEmpty() ? createIfAbsent(rootName)
                                : !isEmpty(tag) ? createIfAbsent(tag)
                                : root);
                        if(fromFile!=null) node.fromFile = fromFile;
                        node.merge(m);
                    }
                    default ->
                        Dlg.error("Bad data in", fn.toString(), Utils.deepToString(v, 80));
                }
                return null;
            });
        } catch(Throwable ex) {
            Dlg.error("Couldn't load "+fn, ex);
        }
    }
    public void initialize() {
        try {
            rootPath = Config.scanConfig("pcat", (a, b) -> load(a, Path.of(b)));
        } catch(Throwable t) {
            Dlg.error("Couldn't scan parts catalog", t);
        }
//        root.dump(0);
    }
    private static final Pattern joiners = Pattern.compile(" *[.,:/] *");
}
