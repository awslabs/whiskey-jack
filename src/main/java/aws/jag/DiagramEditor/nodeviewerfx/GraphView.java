/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.nodeviewerfx;

import aws.jag.DiagramEditor.code.CodeGenerator;
import aws.jag.DiagramEditor.infer.*;
import aws.jag.DiagramEditor.util.Exec;
import aws.jag.DiagramEditor.util.Utils;
import aws.jag.DiagramEditor.util.CommitableWriter;
import aws.jag.DiagramEditor.metadata.MetaNodeTreeModel;
import aws.jag.DiagramEditor.metadata.NodeLibrary;
import aws.jag.DiagramEditor.nodegraph.Arc;
import aws.jag.DiagramEditor.nodegraph.MetaNode;
import aws.jag.DiagramEditor.nodegraph.Graph;
import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.dataformat.yaml.*;
import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.nio.charset.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;
import java.util.prefs.*;
import static java.util.prefs.Preferences.*;
import javafx.application.*;
import javafx.css.*;
import javafx.event.*;
import javafx.fxml.*;
import javafx.scene.*;
import javafx.scene.control.*;
import javafx.scene.input.*;
import javafx.scene.layout.*;
import javafx.stage.*;

public class GraphView extends Graph<NodeView, PortView, ArcView, GraphView> implements Initializable {
    public final static PseudoClass HOVER_PSEUDO_CLASS = PseudoClass.getPseudoClass("hover");
    public GraphView() {
        super(NodeView.class, PortView.class, ArcView.class);
        rootWindow = this;
    }
    @Override
    public String getDescription() {
        return "A view on a graph";
    }
    @Override
    public String getName() {
        return "View";
    }
    @FXML
    private AnchorPane view;
    public static Consumer<GraphView> createNotifier;
    @FXML
    private ContextMenu contextMenu;
    @FXML
    private ScrollPane scrollPane;
    @FXML
    private TreeView navTree;
    @FXML
    private javafx.scene.Node keyboardRoot;
    private static GraphView rootWindow;
    static final Preferences pref = userNodeForPackage(GraphView.class);
    private Selectable hovered;
    public javafx.scene.Node dragNode;
//    public final Map<String, NodeView> nByUid = new ConcurrentHashMap<>();
    public final MetaNodeTreeModel mNodeTreeModel = new MetaNodeTreeModel();
    @Override
    public void appendRefTo(StringBuilder sb) {
        sb.append("GraphView?");
    }

    @Override
    public void initialize(URL url, ResourceBundle rb) {
        Thread.setDefaultUncaughtExceptionHandler((t, error) ->
                Dlg.error("In " + t.getName(), error));
        NodeLibrary.singleton.initialize();
        mNodeTreeModel.initialize(navTree, this);
        NodeLibrary.singleton.forAll(n -> {
            if(n.hasPorts()) {
                var namePath = toStringArray("Add", n);
                addMenu(n, namePath);
            }
        });
        var run = new MenuItem("Run");
        run.setOnAction(ae -> new CodeGenerator().Scan(this));
        run.setAccelerator(KeyCombination.valueOf("Shortcut+R"));
        contextMenu.getItems().add(run);
        var infer = new MenuItem("Fix");
        infer.setOnAction(ae -> new InferIntermediates().Scan(this));
        infer.setAccelerator(KeyCombination.valueOf("Shortcut+F"));
        contextMenu.getItems().add(infer);
        var fileActions = new Menu("File");
        contextMenu.getItems().add(fileActions);
        fileActions.getItems().addAll(
                mkaction("Open...", this::openAction, KeyCode.O),
                mkaction("Save", this::saveAction, KeyCode.S),
                mkaction("Save As..", this::saveAsAction, null),
                mkaction("New", this::newAction, KeyCode.N),
                mkaction("Layout", e -> layoutNodes(), KeyCode.L),
                mkaction("Export All Meta", NodeLibrary.singleton::exportAction, KeyCode.X),
                mkaction("Quit", this::quitAction, KeyCode.Q)
        );
        scrollPane.viewportBoundsProperty().addListener(b -> {
            var z = scrollPane.getViewportBounds();
            scrollPane.setFitToWidth(getView().prefWidth(-1) < z.getWidth());
            scrollPane.setFitToHeight(getView().prefHeight(-1) < z.getHeight());
        });
        scrollPane.setOnMousePressed(mouseEvent -> {
            // record a delta distance for the drag and drop operation.
            DragAssist.targetX = mouseEvent.getScreenX();
            DragAssist.targetY = mouseEvent.getScreenY();
        });
        keyboardRoot.setOnKeyPressed(e -> keyTyped(e));
        openDefault();
        getView().getStyleClass().add("baseLayer");
        getView().setOnDragOver(evt -> {
            evt.acceptTransferModes(TransferMode.ANY);
            evt.consume();
        });
        getView().setOnDragDropped(evt -> {
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
        System.out.println("Typed " + c);
        switch(c.getCode()) {
            default -> {
                return;
            }
            case DELETE, BACK_SPACE -> {
                System.out.println("  DEL " + getHovered());
                forEachSelected(s -> s.delete());
                clearSelection();
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
            m.setAccelerator(new KeyCodeCombination(code, KeyCombination.META_DOWN));
        m.setOnAction(evt);
        return m;
    }
    public static Window rootWindow() {
        if(rootWindow == null) return null;
        var s = rootWindow.getView().getScene();
        return s == null ? null : s.getWindow();
    }
    private static final Path dfltFile = Exec.deTilde("~/untitled." + graphFileExtension);
    private static String currentFile;
    void openAction(ActionEvent evt) {
        var preferred = new File(currentFile != null ? currentFile : pref.get("lastFile", dfltFile.toString()));
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Open Architecture Diagram");
        fileChooser.setInitialDirectory(preferred.getParentFile());
        fileChooser.setInitialFileName(preferred.getName());
        fileChooser.getExtensionFilters().addAll(
                new FileChooser.ExtensionFilter("Architecture Diagrams", "*." + graphFileExtension),
                new FileChooser.ExtensionFilter("All Files", "*.*"));
        var selectedFile = fileChooser.showOpenDialog(rootWindow()).toString();
        pref.put("lastFile", selectedFile);
        if(selectedFile != null)
            loadFile(selectedFile);

    }
    void openDefault() {
        if(loadFile(pref.get("lastFile", null)))
            return;
        if(loadFile(dfltFile.toString()))
            return;
        loadFile(this.getClass().getResource("/ang/unknown.ang"));
    }
    void saveAction(ActionEvent evt) {
        if(currentFile == null || currentFile.startsWith("untitled"))
            saveAsAction(evt);
        else saveFile(currentFile);
    }
    void saveAsAction(ActionEvent evt) {
        var preferred = new File(currentFile != null ? currentFile : pref.get("lastFile", dfltFile.toString()));
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Save Architecture Diagram");
        fileChooser.setInitialDirectory(preferred.getParentFile());
        fileChooser.setInitialFileName(preferred.getName());
        fileChooser.getExtensionFilters().addAll(
                new FileChooser.ExtensionFilter("Architecture Diagrams", "*." + graphFileExtension),
                new FileChooser.ExtensionFilter("All Files", "*.*"));
        saveFile(fileChooser.showSaveDialog(rootWindow()).toString());
    }
    boolean saveFile(String file) {
        if(file != null)
            try(var w = CommitableWriter.abandonOnClose(Path.of(file))) {
            System.out.println("Writing " + dfltFile);
            fileio.writeValue(w, collect());
            w.commit();
            currentFile = file;
            pref.put("lastFile", file);
            note("Saved", file);
            return true;
        } catch(IOException ioe) {
            error("Save failed", file, ioe);
            Utils.getUltimateCause(ioe).printStackTrace(System.out);
            return false;
        } else return true;
    }
    void quitAction(ActionEvent evt) {
        ((Stage) scrollPane.getScene().getWindow()).close();
//        System.exit(0);
    }
    void newAction(ActionEvent evt) {
        clearAll();
    }
    public boolean loadFile(String p) {
        if(p != null) try {
            var ret = loadFile(new File(p).toURI().toURL());
            currentFile = p;
            return ret;
        } catch(MalformedURLException ex) {
        }
        return false;
    }
    public boolean loadFile(URL p) {
        System.out.println("Trying " + p);
        if(p == null) return false;
        clearConnections();
        try(var in = new InputStreamReader(p.openStream(), StandardCharsets.UTF_8)) {
            for(var n: fileio.readValue(in, Object[].class))
                if(n instanceof Map m)
                    add(NodeView.of(m, this));
            forEachConnection(pc -> {
                System.out.println("Connection " + pc);
                pc.from().connectTo(get(pc.toUid()).getPort(pc.toPort()));
            });
            adjustArcs();
            System.out.println("Loaded " + p);
            return true;
        } catch(IOException ioe) {
            error(ioe);
            ioe.printStackTrace(System.out);
            return false;
        }
    }
    public NodeView make(MetaNode n) {
//        System.out.println("Creating " + n);
        var nodeView = new NodeView(this, n);
        var pane = nodeView.getView();
        pane.setUserData(nodeView);
//        getView().getChildren().add(pane);
        ix++;
        if(getHovered() instanceof Arc arc) {
            var in0 = arc.inOutPort(true);
            var in = nodeView.defaultPort(true);
            var out0 = arc.inOutPort(false);
            var out = nodeView.defaultPort(false);
            arc.delete();
            if(out0 != null && in != null)
                newArc((PortView) out0, (PortView) in);
            if(out != null && in0 != null)
                newArc((PortView) out, (PortView) in0);
            layoutNodes();
        }
        var lp = getView().screenToLocal(DragAssist.targetX, DragAssist.targetY);
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
//        makeDraggable(pane);
        getView().requestFocus();
        return nodeView;
    }
    @Override
    public void add(NodeView model) {
        nByUid.put(model.getUid(), model);
        super.add(model);
    }
    private final AtomicBoolean adjustArcsQueued = new AtomicBoolean(false);
    public void adjustArcs() {
//        System.out.println("adjustArcs1");
        if(adjustArcsQueued.getAndSet(true))
            return;
//        System.out.println("adjustArcs2");
        Platform.runLater(() -> {
//        System.out.println("adjustArcs3");
            adjustArcsQueued.set(false);
            var t = getView().getLocalToSceneTransform();
            nByUid.values().forEach(n ->
                    n.forEachPort(a -> ((PortView) a).reposition(t)));
        });
    }
    private final AtomicBoolean adjustNamesQueued = new AtomicBoolean(false);
    public void adjustNames() {
        if(!adjustNamesQueued.getAndSet(true))
            Platform.runLater(() -> {
                adjustNamesQueued.set(false);
                nByUid.values().forEach(n ->
                        n.forEachPort(port -> ((PortView) port).setViewText()));
            });
    }
    private final AtomicBoolean checkTypes = new AtomicBoolean(false);
    @Override
    public void checkTypes() {
        if(!checkTypes.getAndSet(true))
            Platform.runLater(() -> {
                adjustNames();
                checkTypes.set(false);
                new TypeCheck().Scan(this);
            });
    }
    private final AtomicBoolean doInferIntermediates = new AtomicBoolean(false);
    @Override
    public void inferIntermediates() {
        if(!doInferIntermediates.getAndSet(true)) {
            checkTypes();
            Platform.runLater(() -> {
                doInferIntermediates.set(false);
                new InferIntermediates().Scan(this);
            });
        }
    }
    private final AtomicBoolean doLayoutNodes = new AtomicBoolean(false);
    @Override
    public void layoutNodes() {
        adjustNames();
        if(!doLayoutNodes.getAndSet(true)) {
            checkTypes();
            Platform.runLater(() -> {
                doLayoutNodes.set(false);
                new Layout(nByUid.values()).trivialLayout().center().apply();
            });
        }
    }
    @Override
    public void error(Object... o) {
        Dlg.error(o);
    }
    @Override
    public void note(Object... o) {
        Dlg.note(o);
    }
    @Override
    public Object collect() {
        var ret = new ArrayList<>();
        getView().getChildren().forEach(n -> {
            var u = n.getUserData();
            if(u instanceof NodeView f)
                ret.add(f.collect());
        });
        return ret;
    }
    public javafx.scene.Node[] allNodeViews() {
        return getView().getChildren().stream()
                .filter(n -> n.getUserData() instanceof NodeView)
                .toArray(n -> new javafx.scene.Node[n]);
    }
    public void clearAll() {
        getView().getChildren().removeAll(allNodeViews());
    }

    static private int ix = 0;
    private static MenuItem find(List<MenuItem> items, String name) {
        for(var mi: items)
            if(mi.getText().equalsIgnoreCase(name))
                return mi;
        return null;
    }

    public static void dump(String label, int depth, javafx.scene.Node node, Set<javafx.scene.Node> skip) {
        if(node != null && !skip.contains(node)) {
            skip.add(node);
            pln(depth, label + " " + node.getClass().getSimpleName());
            node.lookupAll("*").forEach(n ->
                    dump(toString(node.getStyleClass()).toString(), depth + 1, n, skip));
            if(node instanceof Parent p)
                p.getChildrenUnmodifiable().forEach(n ->
                        dump("*", depth + 1, n, skip));
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
    public AnchorPane getView() {
        return view;
    }
    public void clearSelection() {
        selectionSet.forEach(s ->
                s.getView().getStyleClass().remove("selected"));
        selectionSet.clear();
    }
    public void addToSelection(Selectable s) {
        s.getView().getStyleClass().add("selected");
        selectionSet.add(s);
    }
    public void forEachSelected(Consumer<Selectable> func) {
        if(selectionSet.isEmpty()) {
            if(getHovered() != null)
                func.accept(getHovered());
        } else selectionSet.forEach(func);
    }
    private final Set<Selectable> selectionSet = new HashSet<>();
    /**
     * @return the hovered
     */
    public Selectable getHovered() {
        return hovered;
    }
    /**
     * @param hovered the hovered to set
     */
    public void setHovered(Selectable hovered) {
        this.hovered = hovered;
    }
}
