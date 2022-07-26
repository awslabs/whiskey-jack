/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.graph;

import com.aws.jag.fxnodeeditor.meta.*;
import com.aws.jag.fxnodeeditor.util.*;
import static com.aws.jag.fxnodeeditor.util.Utils.*;
import java.util.*;
import javafx.geometry.*;
import javafx.scene.control.*;
import javafx.scene.layout.*;

/** flow-graph node: both view and controller, the model is derived lazily */
public class FGNode extends Collectable {
    public final GridPane contents;
    public final NodeEditorController controller;
    public final ArrayList<InArc> inputs = new ArrayList<>();
    public final MetaNode meta;
    public final ArrayList<OutArc> outputs = new ArrayList<>();
    public final String uid;
    public final NodePane view;
    @SuppressWarnings("LeakingThisInConstructor")
    public FGNode(MetaNode mn, NodeEditorController c, String u) {
        if(u == null)
            u = Utils.generateRandomString(16);
        uid = u;
        meta = mn;
        controller = c;
        contents = new GridPane();
        contents.setPadding(noPadding);
        var left = new ColumnConstraints();
        left.setHalignment(HPos.LEFT);
        left.setFillWidth(true);
        var right = new ColumnConstraints();
        right.setHalignment(HPos.RIGHT);
        right.setFillWidth(true);
        contents.getColumnConstraints().addAll(left, right);
        contents.getStyleClass().add("nodeItem");
        view = new NodePane(mn.name, mn.fullname(), contents, this);
        view.hoverProperty().addListener(b -> {
            controller.hovered = view.isHover() ? this : null;
        });
        view.getStyleClass().add("nodePane");
        mn.forAllChildren(p -> {
            var x = p.in ? 0 : 1;
            var y = p.slot;
            ArcEndpoint endpoint;
            if(p.in) {
                var in = new InArc(p, FGNode.this, null);
                inputs.add(in);
                endpoint = in;
                in.setValue(p.dflt);
            } else {
                var out = new OutArc(p, FGNode.this);
                outputs.add(out);
                endpoint = out;
            }
            contents.add(new PortView(endpoint, this), x, y);
        });
        if(inputs.isEmpty())
            contents.add(new Label("-"), 0, 0);
        c.nByUid.put(u, this);
    }
    
    private static final Insets noPadding = new Insets(0, 0, 0, 0);
    
    public static FGNode of(Map m, NodeEditorController c, Map<FGNode, Map> cx) {
        var uid = Coerce.get(m, "uid", "nouid");
        var prev = c.nByUid.get(uid);
        if(prev != null) {
            Dlg.error("Duplicate " + prev.meta.name, prev.uid);
            return prev;
        }
        var metan = Coerce.get(m, "meta", "nometa");
        var x = Coerce.get(m, "x", 0.0);
        var y = Coerce.get(m, "y", 0.0);
        var expanded = Coerce.get(m, "expanded", true);
        var values = Coerce.getMap(m, "values");
        var ret = new FGNode(c.mnodes.createIfAbsent(metan), c, uid);
        ret.inputs.forEach(a -> {
            var v = values.get(a.meta.name);
            if(v != null)
                a.value = v;
        });
        var connections = Coerce.getMap(m, "connections");
        if(!connections.isEmpty())
            cx.put(ret, connections);
        ret.view.setExpanded(expanded);
        ret.view.setLayoutX(x);
        ret.view.setLayoutY(y);
        return ret;
    }
    public InArc defaultIn() {
        if(inputs.isEmpty()) return null;
        for(var i:inputs)
            if("in".equals(i.meta.name)) return i;
        return inputs.get(0);
    }
    public OutArc defaultOut() {
        if(inputs.isEmpty()) return null;
        for(var i:outputs)
            if("out".equals(i.meta.name)) return i;
        return outputs.get(0);
    }
    public void applyConnections(Map m) {
//        System.out.println("Apply Connections "+meta.name+" "+uid+"\n\t"+Utils.deepToString(m,100));
        inputs.forEach(a -> {
            var v = m.get(a.meta.name);
//            System.out.println("  ?"+a.meta.name+" "+v);
            if(v != null) {
                var un = v.toString();
                var colon = un.indexOf(':');
                if(colon > 0) {
                    var u = un.substring(0, colon);
                    var n = un.substring(colon + 1);
                    if("v".equals(n))
                        n = "out";
                    var fgn = controller.nByUid.get(u);
                    for(var in: fgn.outputs)
                        if(in.meta.name.equals(n))
                            a.setIncoming(in);
                }
            }
        });
    }
    @Override
    public Object collect() {
        var ret = new HashMap<String, Object>();
        ret.put("uid", uid);
        ret.put("meta", meta.fullname());
        var values = new HashMap();
        var conn = new HashMap();
        inputs.forEach(i -> {
            values.put(i.meta.name, i.value);
            if(i.comesFrom != null)
                conn.put(i.meta.name, i.comesFrom.uname);
        });
        if(!values.isEmpty())
            ret.put("values", values);
        if(!conn.isEmpty())
            ret.put("connections", conn);
        ret.put("x", view.getLayoutX());
        ret.put("y", view.getLayoutY());
        ret.put("expanded", view.isExpanded());
        return ret;
    }
    public void delete() {
        var firstInput = defaultIn();
        var out0 = firstInput != null ? firstInput.comesFrom : null;
        var firstOutput = defaultOut();
        var in0 = firstOutput != null && !isEmpty(firstOutput.connectsTo)
                ? firstOutput.connectsTo.get(0) : null;
        inputs.forEach(ia -> ia.setIncoming(null));
        var in = new ArrayList<InArc>();
        outputs.forEach(oa -> in.addAll(oa.connectsTo));
        in.forEach(n -> n.setIncoming(null));
        controller.nodeEditor.getChildren().remove(view);
        controller.nByUid.remove(uid);
//        System.out.println(out0 + " => " + in0);
        if(in0 != null && out0 != null)
            in0.setIncoming(out0);
    }


}
