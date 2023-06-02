/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.metadata;

import aws.WhiskeyJack.util.Collectable;
import aws.WhiskeyJack.util.CommitableReader;
import aws.WhiskeyJack.util.CommitableWriter;
import aws.WhiskeyJack.util.Utils;
import aws.WhiskeyJack.util.Coerce;
import aws.WhiskeyJack.nodegraph.MetaNode;
import aws.WhiskeyJack.nodegraph.Node;
import aws.WhiskeyJack.nodegraph.Type;
import aws.WhiskeyJack.nodegraph.Domain;
import aws.WhiskeyJack.nodeviewerfx.Dlg;
import aws.WhiskeyJack.nodeviewerfx.GraphView;
import io.github.classgraph.*;
import static aws.WhiskeyJack.util.Utils.*;
import java.io.*;
import java.nio.charset.*;
import java.nio.file.*;
import static java.nio.file.Path.*;
import java.util.*;
import java.util.function.*;
import java.util.regex.*;
import javafx.event.*;

/**
 * A library of meta nodes. The "product catalog"
 */
public class NodeLibrary {
    private NodeLibrary() {
    } // use singleton to access
    public void forAll(Consumer<MetaNode> f) {
        MetaNode.metaMeta.forEachLeaf(f);
    }
    public MetaNode createIfAbsent(String name) {
        if(isEmpty(name) || "root".equals(name))
            return MetaNode.metaMeta;
        var names = joiners.split(name);
        var v = MetaNode.metaMeta;
        for(var part: names)
            v = v.createIfAbsent(part);
        return v;
    }
    public void exportAction(ActionEvent t) {
        saveAllDirty();
    }
    public void saveAllAs(Path p) {
        try(var out = CommitableWriter.abandonOnClose(p)) {
            GraphView.fileio.writeValue(out, Collectable.asObject(MetaNode.metaMeta));
            out.commit();
        } catch(IOException ioe) {
            Dlg.error("Can't save file", ioe);
        }
    }
    public void saveAllDirty() {
        var l = new ArrayList<String>();
        MetaNode.cleanAllDirty(m -> {
            l.add(m.getName());
            // TODO
            System.out.println("Should have written dirty " + m);
        });
        if(l.isEmpty())
            Dlg.note("No modified product catalogs");
        else
            Dlg.note("Wrote:", l);
    }
    public void load(String tag, Path fn) {
        try {
            System.out.println("load " + tag + " " + fn);
            CommitableReader.of(fn).read(in -> load(tag, fn, in));
        } catch(IOException ex) {
            Dlg.error("Couldn't load " + fn, ex);
        }
    }
    private static final Map<Node, Path> loadedFrom = Collections.synchronizedMap(new WeakHashMap<>());
    public static Path from(Node n) {
        return loadedFrom.get(n);
    }
    private Void load(String tag, Path from, InputStream in) throws IOException {
        var v = GraphView.fileio.readValue(in, Object.class);
        if(v instanceof Map m) {
            var rootName = Coerce.get(m, "name", "");
            var node = (!rootName.isEmpty() ? createIfAbsent(rootName)
                    : !isEmpty(tag) ? createIfAbsent(tag)
                    : MetaNode.metaMeta);
            if(from != null)
                loadedFrom.put(node, from);
            node.populateFrom(m);
        } else
            Dlg.error("Bad data in", from.toString(), Utils.deepToString(v, 80));
        return null;
    }
    static final Pattern tagPart = Pattern.compile("ang/pcats/*([^.]*)\\.pcat");
    public void initialize() {
        System.out.println("_____\nLoading standard library");
        try(var scanResult = new ClassGraph().acceptPaths("ang/pcats").scan()) {
            scanResult.getResourcesWithExtension("pcat")
                    .getURIs().forEach(uri ->
                    {
                        try {
                            var tag = "unknown";
                            var m = tagPart.matcher(uri.toString());
                            if(m.find()) {
                                tag = m.group(1);
                            } else
                                System.out.println("Unexpected resource: "+uri);
                            var l = uri.toURL();
                            load(tag, null, l.openStream());
                        } catch(IOException ex) {
                            Dlg.error("Couldn't read default parts catalog ", ex);
                        }
                    });
        }
        try {
            Config.scanConfig("pcat", (a, b) -> load(a, Path.of(b)));
        } catch(Throwable t) {
            Dlg.error("Couldn't scan parts catalog", t);
        }
        saveAllAs(HOME_PATH.resolve("dump.pcat"));
    }
    private static final Pattern joiners = Pattern.compile(" *[.,:/] *");
    public static final NodeLibrary singleton = new NodeLibrary();
    private Map<Domain, Map<Type, List<MetaNode>>> dtInMap;
    public void forEachNodeThatTakes(Domain d, Type t, Consumer<MetaNode> f) {
        if(t == Type.any)
            for(var t0: Type.allInteresting())
                getNodesThatTake(d, t0).forEach(f);
        else if(d == Domain.any)
            for(var d0: Domain.allInteresting())
                getNodesThatTake(d0, t).forEach(f);
        else {
            getNodesThatTake(d, t).forEach(f);
            getNodesThatTake(Domain.any, t).forEach(f);
            getNodesThatTake(d, Type.any).forEach(f);
            getNodesThatTake(Domain.any, Type.any).forEach(f);
        }
    }
    private List<MetaNode> getNodesThatTake(Domain d, Type t) {
        var dtim = dtInMap;
        if(dtim == null) {
            dtInMap = dtim = new HashMap<>();
            System.out.println("Init DT map");
            forAll(mn -> {
                var din = mn.defaultPort(true);
                var dout = mn.defaultPort(false);
                if(din != null && dout != null) {
                    if(dout.compatibleWith(din)) {
                        System.out.println("NOP " + mn.getName() + " " + din.getDomain() + "," + din.getType().getName()
                                           + " -> " + dout.getDomain() + "," + dout.getType().getName());
                        return;
                    }
                    var nd = din.getDomain();
                    var nt = din.getType();
                    var dmap = dtInMap.get(nd);
                    if(dmap == null) dtInMap.put(nd, dmap = new HashMap<>());
                    var list = dmap.get(nt);
                    if(list == null) dmap.put(nt, list = new ArrayList<>());
                    list.add(mn);
                }
            });
        }
        var dm = dtim.get(d);
        if(dm == null) return Collections.EMPTY_LIST;
        var ret = dm.get(t);
        return ret == null ? Collections.EMPTY_LIST : ret;
    }
}