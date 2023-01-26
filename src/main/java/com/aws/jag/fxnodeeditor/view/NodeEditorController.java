/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.view;

//import com.aws.jag.fxnodeeditor.graph.*;
import com.aws.jag.fxnodeeditor.gengraph.*;
import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.dataformat.yaml.*;
import com.aws.jag.fxnodeeditor.meta.*;
import com.aws.jag.fxnodeeditor.util.*;
import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.nio.file.*;
import java.util.*;
import java.util.prefs.Preferences;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;
import static java.util.prefs.Preferences.*;
import javafx.application.*;
import javafx.event.*;
import javafx.fxml.*;
import javafx.scene.*;
import javafx.scene.Node;
import javafx.scene.control.*;
import static javafx.scene.input.KeyCode.*;
import javafx.scene.input.*;
import javafx.scene.layout.*;

public class NodeEditorController extends Collectable implements Initializable {
    @FXML
    public AnchorPane nodeEditor;
    public static Consumer<NodeEditorController> createNotifier;
    @FXML
    private ContextMenu contextMenu;
    @FXML
    private ScrollPane scrollPane;
    @FXML
    private TreeView navTree;
    @FXML
    private Node keyboardRoot;
    static final Preferences pref = userNodeForPackage(NodeEditorController.class);
    public Object hovered;
    public Node dragNode;
    GraphView viewedGraph;
    public final Map<String, NodeView> nByUid = new ConcurrentHashMap<>();
    public final MetaNodeTreeModel mNodeTreeModel = new MetaNodeTreeModel();
    @Override
    public void appendRefTo(StringBuilder sb) {
        sb.append("NodeEditorController?");
    }

    @Override
    public void initialize(URL url, ResourceBundle rb) {
        Thread.setDefaultUncaughtExceptionHandler((t, error) -> Dlg.error("In " + t.getName(), error));
        NodeLibrary.singleton.initialize();
//        mNodeTreeModel.initialize(navTree, NodeLibrary.singleton, this);
        NodeLibrary.singleton.forAll(n -> {
            if(n.hasPorts()) {
                var namePath = toStringArray("Add", n);
                addMenu(n, namePath);
            }
        });
        var fileActions = new Menu("File");
        contextMenu.getItems().add(fileActions);
        fileActions.getItems().addAll(
                mkaction("Open", this::openAction, KeyCode.O),
                mkaction("Save", this::saveAction, KeyCode.S),
                mkaction("New", this::newAction, KeyCode.N),
                mkaction("Layout", e -> layoutAction(), KeyCode.L),
                mkaction("Export All Meta", NodeLibrary.singleton::exportAction, KeyCode.X),
                mkaction("Quit", this::quitAction, KeyCode.Q)
        );
        scrollPane.viewportBoundsProperty().addListener(b -> {
            var z = scrollPane.getViewportBounds();
            scrollPane.setFitToWidth(nodeEditor.prefWidth(-1) < z.getWidth());
            scrollPane.setFitToHeight(nodeEditor.prefHeight(-1) < z.getHeight());
        });
        scrollPane.setOnMousePressed(mouseEvent -> {
            // record a delta distance for the drag and drop operation.
            DragAssist.targetX = mouseEvent.getScreenX();
            DragAssist.targetY = mouseEvent.getScreenY();
        });
        keyboardRoot.setOnKeyPressed(e -> keyTyped(e));
        openDefault();
        nodeEditor.getStyleClass().add("baseLayer");
        nodeEditor.setOnDragOver(evt -> {
            evt.acceptTransferModes(TransferMode.ANY);
            evt.consume();
        });
        nodeEditor.setOnDragDropped(evt -> {
            if(DragAssist.createNode != null) {
                DragAssist.targetX = evt.getScreenX();
                DragAssist.targetY = evt.getScreenY();
                make(DragAssist.createNode);
            }
            evt.setDropCompleted(true);
            evt.consume();
        });
//        ViewTest.annotate(nodeEditor);
        if(createNotifier != null)
            createNotifier.accept(this);
    }
    public void setGraph(GraphView vg) {
        if(vg != null && vg != viewedGraph) {
            System.out.println("setGraph " + vg);
            if(viewedGraph != null)
                viewedGraph.controller = null;
            viewedGraph = vg;
            vg.controller = this;
            var children = nodeEditor.getChildren();
            children.clear();
            vg.forEach(n -> children.add(n.getView()));
        }
    }
    private void addMenu(MetaNode n, String[] names) {
        var items = contextMenu.getItems();
        final var limit = names.length - 1;
        for(var i = 0; i < limit; i++) {
            var name = names[i];
            var item = find(items, name);
            if(item instanceof Menu menu)
                items = menu.getItems();
            else if(item == null) {
                var menu = new Menu(name);
                items.add(menu);
                items = menu.getItems();
            }
        }
        var name = names[limit];
        var mi = find(items, name);
        if(mi == null) {
            mi = new MenuItem(name);
            items.add(mi);
        }
        mi.setOnAction(evt -> make(n));
    }
    void keyTyped(KeyEvent c) {
        switch(c.getCode()) {
            default -> {
                return;
            }
            case DELETE, BACK_SPACE -> {
                switch(hovered) {
                    default ->
                        Dlg.error("Hover over a node or arc to delete it");
                    case NodeView n ->
                        n.delete();
//                    case InArc in ->
//                        in.setIncoming(null);
                }
            }
            case HOME -> {
//                switch(hovered) {
//                    default ->
//                        Dlg.error("Hover over a node or arc to edit it");
//                    case NodeView n ->
//                        MetaEditorController.edit(n.meta, this);
//                }
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
    private static final Path dfltFile = Exec.deTilde("~/unknown.ang");
    void openAction(ActionEvent evt) {
        openDefault();
    }
    void openDefault() {
        if(loadFile(pref.get("lastFile", null)))
            return;
        if(loadFile(dfltFile.toString()))
            return;
        loadFile(this.getClass().getResource("/ang/unknown.ang"));
    }
    void saveAction(ActionEvent evt) {

        try( var w = CommitableWriter.abandonOnClose(dfltFile)) {
//            System.out.println("Writing " + dfltFile);
            fileio.writeValue(w, collect());
            w.commit();
        } catch(IOException ioe) {
            Utils.getUltimateCause(ioe).printStackTrace(System.out);
        }
    }
    void quitAction(ActionEvent evt) {
        System.exit(0);
    }
    void newAction(ActionEvent evt) {
        clearAll();
    }
    void layoutAction() {
        new Layout(nByUid.values()).trivialLayout().apply();
    }
    public boolean loadFile(String p) {
//        System.out.println("String "+p);
        if(p != null) try {
            return loadFile(new File(p).toURI().toURL());
        } catch(MalformedURLException ex) {
        }
        return false;
    }
    public boolean loadFile(URL p) {
        System.out.println("Trying " + p);
//        if(p==null) return false;
//        Map<NodeView, Map> connections = new HashMap<>();
//        try( var in = new InputStreamReader(p.openStream(), StandardCharsets.UTF_8)) {
//            for(var n: fileio.readValue(in, Object[].class))
//                if(n instanceof Map m)
//                    add(NodeView.of(m, this, connections));
//            connections.forEach((n, m) -> n.applyConnections(m));
//            adjustArcs();
//            System.out.println("Loaded "+p);
//            return true;
//        } catch(IOException ioe) {
////            ioe.printStackTrace(System.out);
//            return false;
//        }
        return false;
    }
    public NodeView make(MetaNode n) {
//        System.out.println("Creating " + n);
        var view = new NodeView(viewedGraph, n);
        var pane = view.getView();
        pane.setUserData(view);
        nodeEditor.getChildren().add(pane);
        ix++;
        if(hovered instanceof Arc arc) {
            var in0 = arc.inOutPort(true);
            var in = view.defaultPort(true);
            var out0 = arc.inOutPort(false);
            var out = view.defaultPort(false);
            arc.disconnect();
            if(out0 != null && in != null)
                viewedGraph.newArc((PortView) out0, (PortView) in);
            if(out != null && in0 != null)
                viewedGraph.newArc((PortView) out, (PortView) in0);
            Platform.runLater(() -> layoutAction());
        }
        var lp = nodeEditor.screenToLocal(DragAssist.targetX, DragAssist.targetY);
        if(lp != null) {
            pane.setLayoutX(lp.getX());
            pane.setLayoutY(lp.getY());
            var parent = pane.getParent();
            if(parent != null) {
                pane.getParent().applyCss();
                pane.getParent().layout();
                pane.setLayoutX(lp.getX() - pane.getWidth() / 2);
                pane.setLayoutY(lp.getY() - pane.getHeight() / 2);
            }
        }
        makeDraggable(pane);
        nodeEditor.requestFocus();
        return view;
    }
    private void add(NodeView model) {
        try {
            var pane = model.getView();
            pane.setUserData(model);
            nodeEditor.getChildren().add(pane);
            makeDraggable(pane);
        } catch(Throwable t) {
            Dlg.error("Error adding node", t);
        }
    }
    private final AtomicBoolean adjustArcsQueued = new AtomicBoolean(false);
    public void adjustArcs() {
        if(adjustArcsQueued.getAndSet(true))
            return;
        Platform.runLater(() -> {
            adjustArcsQueued.set(false);
            var t = nodeEditor.getLocalToSceneTransform();
            nByUid.values().forEach(n
                    -> n.forEachPort(a -> ((PortView) a).reposition(t)));
        });
    }
    private final AtomicBoolean adjustNamesQueued = new AtomicBoolean(false);
    public void adjustNames() {
        if(!adjustNamesQueued.getAndSet(true))
            Platform.runLater(() -> {
                adjustNamesQueued.set(false);
                nByUid.values().forEach(n
                        -> n.forEachPort(port -> ((PortView) port).setViewText()));
            });
    }
    @Override
    public Object collect() {
        var ret = new ArrayList<>();
        nodeEditor.getChildren().forEach(n -> {
            var u = n.getUserData();
            if(u instanceof NodeView f)
                ret.add(f.collect());
        });
        return ret;
    }
    public Node[] allNodeViews() {
        return nodeEditor.getChildren().stream()
                .filter(n -> n.getUserData() instanceof NodeView)
                .toArray(n -> new Node[n]);
    }
    public void clearAll() {
        nodeEditor.getChildren().removeAll(allNodeViews());
    }

    static private int ix = 0;
    private static MenuItem find(List<MenuItem> items, String name) {
        for(var mi: items)
            if(mi.getText().equalsIgnoreCase(name))
                return mi;
        return null;
    }

    public void makeDraggable(final Node tp) {
        final var dragDelta = new Object() {
            double x, y;
        };
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
    private static String[] toStringArray(Object... o) {
        return appendTo(o, new ArrayList<>()).toArray(n -> new String[n]);
    }
    private static List<String> appendTo(Object o, List<String> l) {
        switch(o) {
            case null -> {
            }
            case Collection c -> {
                for(var e: c)
                    appendTo(e, l);
            }
            case MetaNode m -> {
                if(!m.isRoot()) {
                    appendTo(m.getParent(), l);
                    l.add(m.getName());
                }
            }
            default -> {
                if(o.getClass().isArray()) {
                    var size = Array.getLength(o);
                    for(var i = 0; i < size; i++)
                        appendTo(Array.get(o, i), l);
                } else
                    l.add(o.toString());
            }
        }
        return l;
    }
    static public final ObjectMapper fileio = new ObjectMapper(
            new YAMLFactory()
                    .enable(YAMLGenerator.Feature.MINIMIZE_QUOTES)
                    .enable(YAMLGenerator.Feature.USE_PLATFORM_LINE_BREAKS)
                    .enable(JsonParser.Feature.ALLOW_COMMENTS)
                    .enable(JsonParser.Feature.ALLOW_SINGLE_QUOTES)
                    .enable(JsonParser.Feature.ALLOW_YAML_COMMENTS)
                    .enable(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES)
    ).configure(JsonGenerator.Feature.AUTO_CLOSE_TARGET, false);
}
