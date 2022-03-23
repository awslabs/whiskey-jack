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

public class FGNode {  // flow-graph node
    @SuppressWarnings("LeakingThisInConstructor")
    public FGNode(MNode mn, NodeEditorController c, String u) {
        if(u == null)
            u = Utils.generateRandomString(16);
        uid = u;
        meta = mn;
        controller = c;
        contents = new GridPane();
        contents.setPadding(noPadding);
        contents.setHgap(20);
        var left = new ColumnConstraints();
        left.setHalignment(HPos.LEFT);
        left.setFillWidth(true);
        var right = new ColumnConstraints();
        right.setHalignment(HPos.RIGHT);
        right.setFillWidth(true);
        contents.getColumnConstraints().addAll(left, right);
        contents.getStyleClass().add("nodeItem");
        view = new TitledPane(mn.group + " " + mn.name, contents);
//        view.setAnimated(false);
//        view.expandedProperty().addListener(b -> {
//            controller.adjustArcs();
//            System.out.println("expandedProperty fired");
//        });
//        view.clipProperty().addListener(b->controller.adjustArcs());
//        view.localToSceneTransformProperty().addListener(b -> {
//            controller.adjustArcs();
//        });
//        contents.visibleProperty().addListener(b->controller.adjustArcs());
        view.getStyleClass().add("nodePane");
        mn.ports.forEach(p -> {
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
//            System.out.println(p.name+" "+x+","+y);
        });
        /* WTF  this should work.  It's a mystery
        var watch = !inputs.isEmpty() ? inputs.get(inputs.size()-1)
                : !outputs.isEmpty() ? outputs.get(outputs.size()-1)
                : null;
        if(watch!=null) {
        System.out.println("in "+meta.name+" watching "+watch.meta.name+"  "+watch.view);
        watch.view.localToSceneTransformProperty().addListener(b -> {
        System.out.println(watch.meta.name+" moved");
        controller.adjustArcs();
        });
        }
        else System.out.println("in "+meta.name+" nothing.to watch!!");
        */
        c.nByUid.put(u, this);
    }
    public Map<String, Object> collect() {
        var ret = new HashMap<String, Object>();
        ret.put("uid", uid);
        ret.put("meta", meta.name);
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
    public static FGNode of(Map m, NodeEditorController c) {
        var uid = get(m, "uid", "nouid");
        var metan = get(m, "meta", "nometa");
        var x = get(m, "x", 0.0);
        var y = get(m, "y", 0.0);
        var expanded = get(m, "expanded", true);
        var values = getMap(m, "values");
        var ret = new FGNode(MNode.find(metan), c, uid);
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
    public void applyConnections(Map m) {
        inputs.forEach(a -> {
            var v = m.get(a.meta.name);
            if(v != null) {
//                System.out.println("Linking "+meta.name+"."+a.meta.name+"->"+v);
                var un = v.toString();
                int colon = un.indexOf(':');
                if(colon > 0) {
                    var u = un.substring(0, colon);
                    var n = un.substring(colon + 1);
                    var fgn = controller.nByUid.get(u);
//                    System.out.println("\t"+u+" "+n+": "+fgn);
                    for(var in: fgn.outputs)
                        if(in.meta.name.equals(n))
//                            System.out.println("\t\tBingo!");
                            a.setIncoming(in);
                }
            }
        });
    }
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
    public final String uid;
    public final MNode meta;
    public final NodeEditorController controller;
    public final TitledPane view;
    public final GridPane contents;
    static OutArc dragSource;
    public final ArrayList<InArc> inputs = new ArrayList<>();
    public final ArrayList<OutArc> outputs = new ArrayList<>();
    private static final Insets noPadding = new Insets(0, 0, 0, 0);
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
//                        System.out.println("Clicked "+ae.meta.name);
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
                        var db = evt.getDragboard();
//                        System.out.println("OnDragDropped In " + ea.meta.name);
//                        System.out.println("\tfrom " + db.getContent(nodeFormat));
//                        controller.startDrag(null);
                        evt.setDropCompleted(true);
                        evt.consume();
                    });
                    setOnDragOver(evt -> {
                        if(dragSource.container != FGNode.this) {
//                        System.out.println("DragOver In "+evt.getGestureSource()+"\n\t"+evt.getDragboard());
                            evt.acceptTransferModes(TransferMode.ANY);
//                            getStyleClass().add("good");
                            evt.consume();
                        }
                    });
                    setOnDragEntered(evt -> {
                        if(dragSource.container != FGNode.this) {
                            evt.acceptTransferModes(TransferMode.ANY);
                            getStyleClass().add("good");
//                            System.out.println("DragEntered In " + ea.meta.name);
                            evt.consume();
                        }
                    });
                    setOnDragExited(evt -> {
                        getStyleClass().remove("good");
//                        System.out.println("DragExited In " + ea.meta.name);
                        evt.consume();
                    });
//                    setOnMouseDragEntered(evt -> {
//                        System.out.println("setOnMouseDragEntered In " + ea.meta.name);
//                    });
//                    setOnMouseDragExited(evt -> {
//                        System.out.println("setOnMouseDragExited In " + ea.meta.name);
//                    });
//                    setOnMouseEntered(evt -> {
//                        System.out.println("setOnMouseEntered In " + ea.meta.name);
//                    });
//                    setOnMouseExited(evt -> {
//                        System.out.println("setOnMouseExited In " + ea.meta.name);
//                    });
                }
                case OutArc outa -> {
//                    setOnMousePressed(evt -> {
//                        System.out.println("Pressed Out");
//                        evt.consume();
//                    });
//                    setOnMouseReleased(evt -> {
//                        System.out.println("Released Out");
//                        animTo(evt);
////                        controller.startDrag(null);
//                        dragSource = null;
//                        evt.consume();
//                    });
//                    setOnMouseDragged(evt -> {
////                    System.out.println("Dragged Out");
//                        if(dragSource != null)
//                            animTo(evt);
//                        evt.consume();
//                    });
                    setOnDragDetected(evt -> {
//                        System.out.println("DragDetected");
                        var db = startDragAndDrop(TransferMode.ANY);
                        var content = new ClipboardContent();
                        dragSource = outa;
                        content.put(nodeFormat, dummy);
                        db.setContent(content);
                        evt.consume();
                        db.setDragView(cursor);
//                        var anim = new CubicCurve();
//                        anim.setFill(Color.TRANSPARENT);
//                        anim.setStroke(Color.ORANGE);
//                        anim.setStrokeWidth(2);
//                        var lbl = getBoundsInLocal();
//                        var t = getLocalToSceneTransform();
//                        var sp = t.transform(lbl.getMaxX(), lbl.getHeight() / 2);
//                        var sx = sp.getX();
//                        var sy = sp.getY();
//                        anim.setStartX(sx);
//                        anim.setStartY(sy);
//                        anim.setControlX1(sx + 100);
//                        anim.setControlY1(sy);
//                        controller.startDrag(anim);
//                        animTo(evt);
                    });
//                    setOnDragDone(evt -> {
//                        System.out.println("DragDone Out");
//                    });
//                    setOnDragOver(evt -> {
//                        System.out.println("DragOver");
//                        evt.acceptTransferModes(TransferMode.ANY);
////                        animTo(evt.getX(), evt.getY());
//                        evt.consume();
//                    });
                }
                case default -> {
                }
            }
            ae.setView(this);
        }
//        private void animTo(MouseEvent e) {
//            animTo(e.getSceneX(), e.getSceneY());
//        }
//        private void animTo(double ex, double ey) {
//            if(controller.dragNode instanceof CubicCurve c)
//                try {
////                    var ex = e.getSceneX();
////                    var ey = e.getSceneY();
//                var t = controller.nodeEditor.getLocalToSceneTransform();
//                var inv = t.inverseTransform(ex, ey);
//                c.setEndX(inv.getX());
//                c.setEndY(inv.getY());
//                c.setControlX2(inv.getX() - 100);
//                c.setControlY2(inv.getY());
//            } catch(NonInvertibleTransformException ex1) {
//                ex1.printStackTrace(System.out);
//            }
//        }
        private static final DataFormat nodeFormat = new DataFormat("node arc");
    }
    static final private Image cursor = new Image(FGNode.class.getResourceAsStream("DragTargetCursor.png"));
    static final private Image rightArrow = new Image(FGNode.class.getResourceAsStream("RightArrow.png"));
    private static final Object dummy = new Serializable() {

    };
}
