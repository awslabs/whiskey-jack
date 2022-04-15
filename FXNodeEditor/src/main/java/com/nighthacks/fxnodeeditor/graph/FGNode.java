/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import com.nighthacks.fxnodeeditor.*;
import com.nighthacks.fxnodeeditor.util.*;
import java.io.*;
import java.util.*;
import javafx.geometry.*;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.input.*;
import javafx.scene.layout.*;
import javafx.scene.text.*;

public class FGNode extends Collectable {
    static OutArc dragSource;
    static final private Image closeArrow = new Image(FGNode.class.getResourceAsStream("CloseArrow.png"));
    static final private Image cursor = new Image(FGNode.class.getResourceAsStream("DragTargetCursor.png"));
    private static final Object dummy = new Serializable() {

    };
    private static final Insets noPadding = new Insets(0, 0, 0, 0);
    static final private Image rightArrow = new Image(FGNode.class.getResourceAsStream("RightArrow.png"));
    public static boolean get(Map m, String key, boolean dflt) {
        var v = m.get(key);
        return v == null ? dflt : Coerce.toBoolean(v);
    }
    public static double get(Map m, String key, double dflt) {
        var v = m.get(key);
        return v == null ? dflt : Coerce.toDouble(v);
    }
    public static String get(Map m, String key, String dflt) {
        var v = m.get(key);
        return v == null ? dflt : Coerce.toString(v);
    }
    public static Map getMap(Map m, String key) {
        var v = m.get(key);
        return v instanceof Map vm ? vm : Map.of();
    }
    public static FGNode of(Map m, NodeEditorController c) {
        var uid = get(m, "uid", "nouid");
        var prev = c.nByUid.get(uid);
        if(prev!=null) {
            Dlg.error("Duplicate "+prev.meta.name, prev.uid);
            return prev;
        }
        var metan = get(m, "meta", "nometa");
        var x = get(m, "x", 0.0);
        var y = get(m, "y", 0.0);
        var expanded = get(m, "expanded", true);
        var values = getMap(m, "values");
        var ret = new FGNode(c.mnodes.createIfAbsent(metan), c, uid);
        ret.inputs.forEach(a -> {
            var v = values.get(a.meta.name);
            if(v != null)
                a.value = v;
        });
        var connections = getMap(m, "connections");
        if(!connections.isEmpty())
            c.connections.put(ret, connections);
        ret.view.setExpanded(expanded);
        ret.view.setLayoutX(x);
        ret.view.setLayoutY(y);
        return ret;
    }
    public final GridPane contents;
    public final NodeEditorController controller;
    public final ArrayList<InArc> inputs = new ArrayList<>();
    public final MNode meta;
    public final ArrayList<OutArc> outputs = new ArrayList<>();
    public final String uid;
    public final NodePane view;
    // flow-graph node
    @SuppressWarnings("LeakingThisInConstructor")
    public FGNode(MNode mn, NodeEditorController c, String u) {
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
        view = new NodePane(mn.fullname(), contents);
        view.hoverProperty().addListener(b -> {
            controller.hovered = view.isHover() ? this : null;
        });
        view.getStyleClass().add("nodePane");
        mn.forAllChildren(p -> {
            int x = p.in ? 0 : 1;
            int y = p.slot;
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
            contents.add(new PortView(endpoint), x, y);
        });
        if(inputs.isEmpty())
            contents.add(new Label("-"),0,0);
        c.nByUid.put(u, this);
    }
    public void applyConnections(Map m) {
        inputs.forEach(a -> {
            var v = m.get(a.meta.name);
            if(v != null) {
                var un = v.toString();
                int colon = un.indexOf(':');
                if(colon > 0) {
                    var u = un.substring(0, colon);
                    var n = un.substring(colon + 1);
                    var fgn = controller.nByUid.get(u);
                    for(var in: fgn.outputs)
                        if(in.meta.name.equals(n))
                            a.setIncoming(in);
                }
            }
        });
    }
    public void delete() {
        inputs.forEach(ia->ia.setIncoming(null));
        var in = new ArrayList<InArc>();
        outputs.forEach(oa->in.addAll(oa.goesTo));
        in.forEach(n->n.setIncoming(null));
        controller.nodeEditor.getChildren().remove(view);
        controller.nByUid.remove(uid);
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
    public class NodePane extends VBox {
        private boolean expanded = false;
        private final ImageView openClose;
        private final GridPane contents;
        private final HBox titleRegion;
        NodePane(String title, GridPane c) {
            contents = c;
            var text = new Text(title);
            openClose = new ImageView(closeArrow);
            openClose.setFitHeight(12);
            openClose.setPreserveRatio(true);
            openClose.getStyleClass().add("arrow");
            titleRegion = new HBox(openClose, text);
            HBox.setHgrow(c, Priority.ALWAYS);
            HBox.setHgrow(titleRegion, Priority.ALWAYS);
            HBox.setHgrow(text, Priority.ALWAYS);
            setFillWidth(true);
            getStyleClass().setAll("fgpane");
            titleRegion.getStyleClass().setAll("fgtitle");
            text.getStyleClass().setAll("fgtitletext");
            openClose.getStyleClass().setAll("open");
            getChildren().setAll(titleRegion);
            openClose.setOnMouseReleased(e -> setExpanded(!isExpanded()));
            setExpanded(true);
        }
        public final void setExpanded(boolean b) {
            if(b != expanded) {
                expanded = b;
                openClose.getStyleClass().setAll(expanded ? "fgopen" : "fgclosed");
                getChildren().remove(contents);
                if(expanded)
                    getChildren().add(contents);
                openClose.setRotate(expanded ? 90 : 0);
                resize(getPrefWidth(), expanded ? getPrefHeight() : 30);
                controller.adjustArcs();
            }
        }

        public boolean isExpanded() {
            return expanded;
        }
    }
    public class PortView extends Label {
        @Override
        public String toString() {
            return super.toString() + ":" + getText();
        }
        @SuppressWarnings("LeakingThisInConstructor")
        PortView(ArcEndpoint ae) {
            super(ae.meta.name);
            setGraphicTextGap(4);
            var in = ae.meta.in;
            getStyleClass().add(in ? "inPort" : "outPort");
            var img = new ImageView(rightArrow);
            img.setFitHeight(10);
            img.setPreserveRatio(true);
            img.setCache(true);
            img.setSmooth(true);
            setGraphic(img);
            localToSceneTransformProperty().addListener(b -> {
                controller.adjustArcs();
            });
            setContentDisplay(in ? ContentDisplay.LEFT : ContentDisplay.RIGHT);
            switch(ae) {
                case InArc ea -> {
                    setOnMouseClicked(evt -> {
                        var td = new TextInputDialog(String.valueOf(ea.value));
                        td.setHeaderText(ea.meta.name);
                        td.setTitle("Enter new value");
                        ea.setValue(td.showAndWait());
                    });
                    setOnDragDropped(evt -> {
                        getStyleClass().remove("good");
                        if(dragSource != null) {
                            ea.setIncoming(dragSource);
                            controller.adjustArcs();
                        }
                        evt.setDropCompleted(true);
                        evt.consume();
                    });
                    setOnDragOver(evt -> {
                        if(dragSource.container != FGNode.this) {
                            evt.acceptTransferModes(TransferMode.ANY);
                            evt.consume();
                        }
                    });
                    setOnDragEntered(evt -> {
                        if(dragSource.container != FGNode.this) {
                            evt.acceptTransferModes(TransferMode.ANY);
                            getStyleClass().add("good");
                            evt.consume();
                        }
                    });
                    setOnDragExited(evt -> {
                        getStyleClass().remove("good");
                        evt.consume();
                    });
                }
                case OutArc outa -> {
                    setOnDragDetected(evt -> {
                        var db = startDragAndDrop(TransferMode.ANY);
                        var content = new ClipboardContent();
                        dragSource = outa;
                        content.put(nodeFormat, dummy);
                        db.setContent(content);
                        evt.consume();
                        db.setDragView(cursor);
                    });
                }
                case default -> {
                }
            }
            ae.setView(this);
        }
        private static final DataFormat nodeFormat = new DataFormat("node arc");
    }
}
