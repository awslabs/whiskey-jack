/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodeviewerfx;

import aws.WhiskeyJack.QandA.*;
import aws.WhiskeyJack.code.*;
import aws.WhiskeyJack.infer.*;
import aws.WhiskeyJack.metadata.*;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import static aws.WhiskeyJack.util.Utils.*;
import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;
import java.util.prefs.*;
import static java.util.prefs.Preferences.*;
import javafx.application.*;
import javafx.collections.*;
import javafx.css.*;
import javafx.event.*;
import javafx.fxml.*;
import javafx.geometry.*;
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
    @FXML
    private GridPane propbox;
    @FXML
    private TabPane tabpane;
    private static GraphView rootWindow;
    static final Preferences pref = userNodeForPackage(GraphView.class);
    private final Selection selection = new Selection();
    public javafx.scene.Node dragNode;
    public final MetaNodeTreeModel mNodeTreeModel = new MetaNodeTreeModel();
    private boolean propboxVisible = false;
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
        run.setOnAction(ae ->
                new OverallCodeGenerationDriver().compileEverything(this));
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
                mkaction("Layout", e -> layoutNodes(true), KeyCode.L),
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
        getView().setOnMousePressed(e -> {
            e.consume();
            selection.clear();
        });
        if(createNotifier != null)
            createNotifier.accept(this);

        tabpane.getSelectionModel().selectedIndexProperty().addListener((cl, was, is) ->
        {
            if(propboxVisible = (is.intValue() == 1)) populatePropbox();
        });
        selection.addDomainListener(l -> {
            if(propboxVisible) populatePropbox();
        });
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
        try {
            switch(c.getCode()) {
                default -> {
                    return;
                }
                case DELETE, BACK_SPACE -> {
                    System.out.println("  DEL " + selection.getHovered());
                    selection.forEach(s -> s.delete());
                    selection.clear();
                }
                case T -> {
                    if(selection.getHovered() == null)
                        error("Hover the mouse over an object", "to tag it.");
                    else {
                        var v = Dlg.ask("Tag!", "Enter a tag for this object", "Tag");
                        if(!isEmpty(v))
                            selection.forEach(s -> s.setTag(v));
                    }
                }
                case HOME -> {
                }
            }
        } catch(Throwable t) {
            error(t);
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
        var fileChooser = new FileChooser();
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
        loadFile(this.getClass().getResource("/ang/untitled.ade"));
    }
    void saveAction(ActionEvent evt) {
        if(currentFile == null || currentFile.startsWith("untitled"))
            saveAsAction(evt);
        else saveFile(currentFile);
    }
    void saveAsAction(ActionEvent evt) {
        var preferred = new File(currentFile != null ? currentFile : pref.get("lastFile", dfltFile.toString()));
        var fileChooser = new FileChooser();
        fileChooser.setTitle("Save Architecture Diagram");
        fileChooser.setInitialDirectory(preferred.getParentFile());
        fileChooser.setInitialFileName(preferred.getName());
        fileChooser.getExtensionFilters().addAll(
                new FileChooser.ExtensionFilter("Architecture Diagrams", "*." + graphFileExtension),
                new FileChooser.ExtensionFilter("All Files", "*.*"));
        saveFile(fileChooser.showSaveDialog(rootWindow()).toString());
    }
    boolean saveFile(String file) {
        if(DataIO.yaml.write(collect(), Path.of(file))) {
            currentFile = file;
            pref.put("lastFile", file);
            note("Saved", file);
            return true;
        } else return false;
    }
    void quitAction(ActionEvent evt) {
        ((Stage) scrollPane.getScene().getWindow()).close();
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
        var received = DataIO.yaml.read(p);
        if(received == null) return false;
        clearConnections();
        addRoot(received);
        forEachConnection(pc -> {
            pc.from().connectTo(get(pc.toUid()).getPort(pc.toPort()));
        });
        adjustArcs();
        System.out.println("Loaded " + p);
        setSrc("file".equals(p.getProtocol())
                ? Path.of(p.getPath())
                : Exec.deTilde("~/untitled.ade"));
        return true;
    }

    public final Map<Domain, DomainView> domains = new HashMap<>();
    DomainView getDomainView(Domain d) {
        return domains.computeIfAbsent(d, D -> new DomainView(this, D));
    }
    public void changeDomain(NodeView n, Domain from, Domain to) {
        if(n != null /*&& n.getDomain()!=to*/) {
//            System.out.println("Change domain " + n.getName() + "  " + from + "->" + to);
            if(from != null) getDomainView(from).remove(n);
            if(to != null) getDomainView(to).add(n);
        }
    }
    public NodeView make(MetaNode n) {
        var nodeView = new NodeView(this, n);
        var pane = nodeView.getView();
        pane.setUserData(nodeView);
        ix++;
        if(selection.getHovered() instanceof Arc arc) {
            var in0 = arc.inOutPort(true);
            var in = nodeView.defaultPort(true);
            var out0 = arc.inOutPort(false);
            var out = nodeView.defaultPort(false);
            arc.delete();
            if(out0 != null && in != null)
                newArc((PortView) out0, (PortView) in);
            if(out != null && in0 != null)
                newArc((PortView) out, (PortView) in0);
            layoutNodes(false);
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
        getView().requestFocus();
        return nodeView;
    }
    @Override
    public void remove(NodeView n) {
//        System.out.println("REMOVE?? " + n.getName() + "  " + callers(0, 4));
        changeDomain(n, n.getDomain(), null);
        super.remove(n);
    }
    @Override
    public void add(NodeView n) {
        nByUid.put(n.getUid(), n);
        super.add(n);
        changeDomain(n, null, n.getDomain());
    }
    private void addRoot(Object o) {
        switch(o) {
            case Collection c -> c.forEach(v -> add(v));
            case Map m -> {
                QuestionsByTag.populateFrom(getMap(m, "parameters"));
                add(m.get("nodes"));
            }
            case null -> {
            }
            default ->
                System.out.println("Unexpected GV add: " + o.getClass() + " " + o);
        }
    }
    private void add(Object o) {
        switch(o) {
            case Collection c -> c.forEach(v -> add(v));
            case Map m -> add(NodeView.of(m, this));
            case null -> {
            }
            default ->
                System.out.println("Unexpected GV add: " + o.getClass() + " " + o);
        }
    }
    private final AtomicBoolean adjustArcsQueued = new AtomicBoolean(false);
    public void adjustArcs() {
        if(adjustArcsQueued.getAndSet(true))
            return;
        Platform.runLater(() -> {
            adjustArcsQueued.set(false);
            var t = getView().getLocalToSceneTransform();
            nByUid.values().forEach(n ->
                    n.forEachPort(a -> ((PortView) a).reposition(t)));
            domains.values().forEach(d -> d.reposition());
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
    public void layoutNodes(boolean force) {
        if(force || Question.question("autolayout").isTrue()) {
            adjustNames();
            if(!doLayoutNodes.getAndSet(true)) {
                checkTypes();
                Platform.runLater(() -> {
                    doLayoutNodes.set(false);
                    new Layout(nByUid.values()).trivialLayout().center().apply();
                });
            }
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
        var ret = new HashMap<String, Object>();
        ret.put("parameters", QuestionsByTag.collect());
        var nodes = new ArrayList();
        getView().getChildren().forEach(n -> {
            var u = n.getUserData();
            if(u instanceof aws.WhiskeyJack.nodegraph.Node f)
                nodes.add(f.collect());
        });
        ret.put("nodes", nodes);
        return ret;
    }
    public javafx.scene.Node[] allNodeViews() {
        return getView().getChildren().stream()
                .filter(n -> n.getUserData() instanceof NodeView)
                .toArray(n -> new javafx.scene.Node[n]);
    }
    public void makeDraggable(Selectable s) {
        final var dragInfo = new Object() {
            double x0;
            double y0;
            boolean dragging;
        };
        final var tp = s.getView();
        tp.setOnMousePressed(mouseEvent -> {
            // record a delta distance for the drag and drop operation.
            dragInfo.x0 = mouseEvent.getSceneX();
            dragInfo.y0 = mouseEvent.getSceneY();
            dragInfo.dragging = false;
            tp.setCursor(Cursor.OPEN_HAND);
            mouseEvent.consume();
            if(!mouseEvent.isShiftDown()) selection.clear();
            selection.add(s);
        });
        tp.setOnMouseReleased(mouseEvent -> {
            tp.setCursor(Cursor.HAND);
            mouseEvent.consume();
            if(!dragInfo.dragging)
                selection.add(s);
            else
                selection.forEach(sel -> sel.endDrag());
//            if(!mouseEvent.isShiftDown()) selection.clear();
        });
        tp.setOnMouseDragged(mouseEvent -> {
            if(dragInfo.dragging || selection.canDrag()) {
                dragInfo.dragging = true;
                var dx = mouseEvent.getSceneX() - dragInfo.x0;
                var dy = mouseEvent.getSceneY() - dragInfo.y0;
                selection.forEach(sel -> sel.setDrag(dx, dy));
                adjustArcs();
            }
            mouseEvent.consume();
        });
        tp.setOnMouseEntered(mouseEvent -> {
            if(!mouseEvent.isPrimaryButtonDown())
                tp.setCursor(Cursor.HAND);
            selection.setHovered(s);
            mouseEvent.consume();
        });
        tp.setOnMouseExited(mouseEvent -> {
            if(!mouseEvent.isPrimaryButtonDown())
                tp.setCursor(Cursor.DEFAULT);
            mouseEvent.consume();
            selection.setHovered(null);
        });
    }
    public Selection getSelection() {
        return selection;
    }
    @Override
    public void clearAll() {
        super.clearAll();
        getView().getChildren().clear();
        selection.clear();
        domains.clear();
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
    public AnchorPane getView() {
        return view;
    }
    private void populatePropbox() {
        Collection<javafx.scene.Node> props = new ArrayList<>();
        var s = getSelection().getFirstSelected();
        var domain = s == null ? Domain.any : s.getDomain();
        System.out.println("Populate propbox domain " + domain);
        var row = 0;
        for(var q: Question.extract(q -> q.getDomain() == domain)) {
            Region n;
            var label = new Label(q.get("label", "No label"));
            var type = q.get("type", (Object) null);
            if(type == null) type = "string";
            if(type instanceof Collection c) {
                var ol = FXCollections.observableArrayList(c);
                var b = new ChoiceBox(ol);
                var vpos = ol.indexOf(q.getValue());
                var sel = b.getSelectionModel();
                sel.select(vpos >= 0 ? vpos : 0);
                sel.selectedItemProperty().addListener((cl, was, is) -> {
//                    System.out.println("Changed choice " + is.toString() + " " + cl);
                    q.setValue(is.toString());
                });
                n = b;
            } else switch(type.toString()) {
                case "boolean" -> {
                    var b = new CheckBox();
                    b.setSelected(Coerce.toBoolean(q.getValue()));
                    b.selectedProperty().addListener((cl, was, is) -> {
//                        System.out.println("Changed bool " + is + " " + cl);
                        q.setValue(is);
                    });
                    n = b;
                }
                case "int" -> {
                    var b = new Slider(0, 1, Coerce.toDouble(q.getValue()));
                    b.valueProperty().addListener((cl, was, is) -> {
//                        System.out.println("Changed int " + is + " " + cl);
                        q.setValue(is);
                    });
                    n = b;
                }
                default -> {
                    var b = new TextField(Coerce.toString(q.getValue()));
                    b.textProperty().addListener((cl, was, is) -> {
//                        System.out.println("Changed string " + is + " " + cl);
                        q.setValue(is);
                    });
                    n = b;
                }

            }
            label.setMaxWidth(Double.MAX_VALUE);
            n.setMaxWidth(Double.MAX_VALUE);
            GridPane.setConstraints(label, 0, row, 1, 1, HPos.LEFT, VPos.BASELINE, Priority.ALWAYS, Priority.NEVER);
            GridPane.setConstraints(n, 1, row, 1, 1, HPos.CENTER, VPos.BASELINE, Priority.ALWAYS, Priority.NEVER);
            row++;
            props.add(label);
            props.add(n);
        }
        propbox.getChildren().setAll(props);
        propbox.setMaxWidth(Double.MAX_VALUE);
        var c1 = new ColumnConstraints();
        c1.setPercentWidth(60);
        var c2 = new ColumnConstraints();
        c2.setPercentWidth(40);
        c2.setHgrow(Priority.ALWAYS);
        propbox.getColumnConstraints().setAll(c1, c2);
        propbox.getChildren().setAll(props);
    }
}
