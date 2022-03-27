/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor;

import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.dataformat.yaml.*;
import com.nighthacks.fxnodeeditor.graph.*;
import com.nighthacks.fxnodeeditor.util.*;
import java.io.*;
import java.net.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import javafx.application.*;
import javafx.event.*;
import javafx.fxml.*;
import javafx.scene.*;
import javafx.scene.control.*;
import javafx.scene.input.*;
import javafx.scene.layout.*;

public class NodeEditorController implements Initializable {
    @FXML
    public AnchorPane nodeEditor;
    @FXML
    private ContextMenu contextMenu;
    @FXML
    private ScrollPane scrollPane;
    public Object hovered;
    public Node dragNode;
    double pressX, pressY;
    public final Map<String, FGNode> nByUid = new ConcurrentHashMap<>();
    public final Map<FGNode, Map> connections = new HashMap<>();

    @Override
    public void initialize(URL url, ResourceBundle rb) {
//        System.out.println("initialized " + url + "\n\tnodeEditor=" + nodeEditor);
        MNode.forEach(n -> add(
                evt -> make(n),
                new String[]{"Add", n.group, n.name}));
        var fileActions = new Menu("File");
        contextMenu.getItems().add(fileActions);
        fileActions.getItems().addAll(
                mkaction("Open", openAction, KeyCode.O),
                mkaction("Save", saveAction, KeyCode.S),
                mkaction("New", newAction, KeyCode.N),
                mkaction("Layout", layoutAction, KeyCode.L),
                mkaction("Quit", quitAction, KeyCode.Q)
        );
        scrollPane.viewportBoundsProperty().addListener(b -> {
            var z = scrollPane.getViewportBounds();
            scrollPane.setFitToWidth(nodeEditor.prefWidth(-1) < z.getWidth());
            scrollPane.setFitToHeight(nodeEditor.prefHeight(-1) < z.getHeight());
        });
        scrollPane.setOnMousePressed(mouseEvent -> {
            // record a delta distance for the drag and drop operation.
            pressX = mouseEvent.getScreenX();
            pressY = mouseEvent.getScreenY();
//            System.out.println("Press " + pressX + "," + pressY);
        });
        scrollPane.setOnKeyPressed(e -> keyTyped(e));
        loadFile(dfltFile);
        nodeEditor.getStyleClass().add("baseLayer");
    }
    private void add(EventHandler<ActionEvent> event, String... names) {
        var items = contextMenu.getItems();
        final int limit = names.length - 1;
        for(int i = 0; i < limit; i++) {
            var name = names[i];
            var item = find(items, name);
            if(item instanceof Menu menu)
                items = menu.getItems();
            else if(item == null) {
                var menu = new Menu(name);
                items.add(menu);
                items = menu.getItems();
            } else
                System.out.println("Menu expected at " + name);
//            System.out.println(name+"/ "+items);
        }
        var name = names[limit];
        var mi = find(items, name);
        if(mi == null) {
            mi = new MenuItem(name);
            items.add(mi);
        }
        mi.setOnAction(event);
//        System.out.println(name+"  "+mi);
    }
    void keyTyped(KeyEvent c) {
        System.out.println("Typed " + c.getText() + " " + c.getCharacter() + " " + c.getCode() + "\n\t" + c);
        switch(c.getCode()) {
            default -> {
                return;
            }
            case DELETE, BACK_SPACE -> {
                if(hovered != null) {
                    System.out.println("Hovering over " + hovered);
                    switch(hovered) {
                        case null -> {
                                System.out.println("Hover? NULL");
                        }
                        default -> {
                                System.out.println("Hover? "+hovered);
                        }
                        case FGNode n -> n.delete();
                        case InArc in ->
                            in.setIncoming(null);
                    }
                } else
                    System.out.println("Nothing to delete");
            }
        }
        c.consume();
    }
    private MenuItem mkaction(String name, EventHandler<ActionEvent> evt, KeyCode code) {
        var m = new MenuItem(name);
        if(code != null)
            m.setAccelerator(new KeyCodeCombination(code, KeyCombination.META_ANY));
        m.setOnAction(evt);
        return m;
    }
    private static final Path dfltFile = Exec.deTilde("~/nodes.yaml");
    EventHandler<ActionEvent> openAction = evt -> {
        System.out.println("openAction");
        loadFile(dfltFile);
    };
    EventHandler<ActionEvent> saveAction = evt -> {
        try( var w = CommitableWriter.abandonOnClose(dfltFile)) {
            System.out.println("Writing " + dfltFile);
            fileio.writeValue(w, collect());
            w.commit();
        } catch(IOException ioe) {
            Utils.getUltimateCause(ioe).printStackTrace(System.out);
        }
    };
    EventHandler<ActionEvent> quitAction = evt -> {
        System.exit(0);
    };
    EventHandler<ActionEvent> newAction = evt -> {
        clearAll();
    };
    EventHandler<ActionEvent> layoutAction = evt -> {
        new Layout(nByUid.values()).run();
    };
    public void loadFile(Path p) {
        System.out.println("Loading " + p);
        connections.clear();
        try( var in = Files.newBufferedReader(p)) {
            for(var n: fileio.readValue(in, Object[].class))
                if(n instanceof Map m)
                    add(FGNode.of(m, this));
        } catch(IOException ioe) {
            ioe.printStackTrace(System.out);
        }
        connections.forEach((n, m) -> n.applyConnections(m));
        connections.clear();
        adjustArcs();
    }
    private FGNode make(MNode n) {
        System.out.println("Creating " + n);
        var model = new FGNode(n, NodeEditorController.this, null);
        var pane = model.view;
        pane.setUserData(model);
        nodeEditor.getChildren().add(pane);
        ix++;
        var lp = nodeEditor.screenToLocal(pressX, pressY);
//        System.out.println("lp=" + lp + "   " + pressX + "," + pressY);
        pane.setLayoutX(lp.getX());
        pane.setLayoutY(lp.getY());
        makeDraggable(pane);
        return model;
    }
    private void add(FGNode model) {
//        System.out.println("Adding " + model);
        var pane = model.view;
        pane.setUserData(model);
        nodeEditor.getChildren().add(pane);
        makeDraggable(pane);
    }
    private final AtomicBoolean adjustQueued = new AtomicBoolean(false);
    public void adjustArcs() {
        if(adjustQueued.getAndSet(true))
            return;
        Platform.runLater(() -> {
            adjustQueued.set(false);
            var t = nodeEditor.getLocalToSceneTransform();
            nByUid.values().forEach(n
                    -> n.inputs.forEach(a -> a.reposition(t)));
        });
    }
    public List<Map> collect() {
        var ret = new ArrayList<Map>();
        nodeEditor.getChildren().forEach(n -> {
            var u = n.getUserData();
            if(u instanceof FGNode f)
                ret.add(f.collect());
        });
        return ret;
    }
    public Node[] allFGNodes() {
        return nodeEditor.getChildren().stream()
                .filter(n -> n.getUserData() instanceof FGNode)
                .toArray(n -> new Node[n]);
    }
    public void clearAll() {
        nodeEditor.getChildren().removeAll(allFGNodes());
    }

    static private int ix = 0;
    private static MenuItem find(List<MenuItem> items, String name) {
        for(MenuItem mi: items)
            if(mi.getText().equalsIgnoreCase(name))
                return mi;
        return null;
    }
//    private static String fullName(Object o) {
//        return o instanceof MenuItem ? fullName((MenuItem) o) : String.valueOf(o);
//    }
//    private static String fullName(MenuItem o) {
//        if(o==null)
//            return null;
//        var parent = fullName(o.getParentMenu());
//        var myName = o.getText();
//        return parent==null||parent.isBlank() ? myName : parent+"."+myName;
//    }

    public void makeDraggable(final Node tp) {
        final var dragDelta = new Object() {
            double x, y;
        };
//        var tp = tp.getContent();
//        dump("root",1,byNode.getParent(), new HashSet<>());
//        dump("arrow",1,byNode.lookup(".arrow-button"), new HashSet<>());
        tp.setOnMousePressed(mouseEvent -> {
            // record a delta distance for the drag and drop operation.
            dragDelta.x = tp.getLayoutX() - mouseEvent.getScreenX();
            dragDelta.y = tp.getLayoutY() - mouseEvent.getScreenY();
            tp.setCursor(Cursor.OPEN_HAND);
            mouseEvent.consume();
        });
        tp.setOnMouseReleased(mouseEvent -> {
            tp.setCursor(Cursor.HAND);
            mouseEvent.consume();
        });
        tp.setOnMouseDragged(mouseEvent -> {
            tp.setLayoutX(mouseEvent.getScreenX() + dragDelta.x);
            tp.setLayoutY(mouseEvent.getScreenY() + dragDelta.y);
            adjustArcs();
            mouseEvent.consume();
        });
        tp.setOnMouseEntered(mouseEvent -> {
            if(!mouseEvent.isPrimaryButtonDown())
                tp.setCursor(Cursor.HAND);
            mouseEvent.consume();
        });
        tp.setOnMouseExited(mouseEvent -> {
            if(!mouseEvent.isPrimaryButtonDown())
                tp.setCursor(Cursor.DEFAULT);
            mouseEvent.consume();
        });
    }
    public static void dump(String label, int depth, Node node, Set<Node> skip) {
        if(node != null && !skip.contains(node)) {
            skip.add(node);
            pln(depth, label + " " + node.getClass().getSimpleName());
//            node.getProperties().forEach((k,v)->pln(depth+1,k+": "+v));
            node.lookupAll("*").forEach(n -> dump(toString(node.getStyleClass()).toString(), depth + 1, n, skip));
            if(node instanceof Parent p)
                p.getChildrenUnmodifiable().forEach(n -> dump("*", depth + 1, n, skip));
        }
    }
    private static void pln(int depth, CharSequence s) {
        var sb = new StringBuilder();
        while(--depth >= 0)
            sb.append("    ");
        sb.append(s);
        sb.append('\n');
        System.out.append(sb);
        System.out.flush();
    }
    private static CharSequence toString(List<String> s) {
        var sb = new StringBuilder();
        s.forEach(e -> {
            if(sb.length() > 0)
                sb.append(',');
            sb.append(e);
        });
        return sb;
    }
    static ObjectMapper fileio = new ObjectMapper(
            new YAMLFactory()
                    .enable(YAMLGenerator.Feature.MINIMIZE_QUOTES)
                    .enable(YAMLGenerator.Feature.USE_PLATFORM_LINE_BREAKS)
                    .enable(JsonParser.Feature.ALLOW_COMMENTS)
                    .enable(JsonParser.Feature.ALLOW_SINGLE_QUOTES)
                    .enable(JsonParser.Feature.ALLOW_YAML_COMMENTS)
                    .enable(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES)
    ).configure(JsonGenerator.Feature.AUTO_CLOSE_TARGET, false);
//    public void startDrag(Node n) {
//        var d = dragNode;
//        if(d!=null) {
//            d.setVisible(false);
//            nodeEditor.getChildren().remove(d);
//        }
//        if(n!=null)
//            nodeEditor.getChildren().add(dragNode = n);
//    }
}
