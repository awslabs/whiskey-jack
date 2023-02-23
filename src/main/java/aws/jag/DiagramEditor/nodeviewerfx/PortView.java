/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.jag.DiagramEditor.nodeviewerfx;

import aws.jag.DiagramEditor.nodegraph.Node;
import aws.jag.DiagramEditor.nodegraph.Port;
import aws.jag.DiagramEditor.nodegraph.MetaPort;
import javafx.geometry.*;
//import com.aws.jag.fxnodeeditor.graph.*;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.input.*;
import javafx.scene.transform.*;
import javax.annotation.*;

public class PortView extends Port implements Selectable {
    private Label view;
    public PortView(@Nonnull Node parent, @Nonnull PortView original) {
        super(parent, original.metadata);
        populateFrom(original);
        init();
    }
    public PortView(@Nonnull NodeView parent, @Nonnull MetaPort mn) {
        super(parent, mn);
        init();
    }
    @Override
    public void delete() {
        // TODO com.aws.jag.fxnodeeditor.view.PortView.delete Not implemented
        Dlg.error("Ports cannot be deleted from their node");
    }
    @Override
    public GraphView getContext() {
        return (GraphView)super.getContext();
    }

    @SuppressWarnings(value="LeakingThisInConstructor")
    private void init() { 
        view = new Label("");
        setViewText();
        view.setGraphicTextGap(4);
        var in = metadata.isRightSide();
        view.getStyleClass().add(in ? "inPort" : "outPort");
        var img = new ImageView(rightArrow);
        img.setFitHeight(10);
        img.setPreserveRatio(true);
        img.setCache(true);
        img.setSmooth(true);
        view.setGraphic(img);
        view.localToSceneTransformProperty().addListener(b -> {
            getContext().adjustArcs();
        });
        view.setContentDisplay(in ? ContentDisplay.LEFT : ContentDisplay.RIGHT);
//        if(in) {
                if(in) view.setOnMouseClicked(evt -> {
                    TextInputDialog td = new TextInputDialog(String.valueOf(getValue()));
                    td.setHeaderText(metadata.getName());
                    td.setTitle("Enter new value");
                    setValue(td.showAndWait().get());
                });
                view.setOnDragDetected(evt -> {
                    DragAssist.dragClean();
                    javafx.scene.input.Dragboard db = view.startDragAndDrop(TransferMode.ANY);
                    javafx.scene.input.ClipboardContent content = new ClipboardContent();
                    DragAssist.createArc = this;
                    content.put(DragAssist.draggingOutArc, DragAssist.dummy);
                    db.setContent(content);
                    evt.consume();
                    db.setDragView(cursor);
                });
                view.setOnDragOver(evt -> {
                    if(DragAssist.createNode != null || DragAssist.createArc != null && DragAssist.createArc.within != within) {
                        evt.acceptTransferModes(TransferMode.ANY);
                        evt.consume();
                    }
                });
                view.setOnDragEntered(evt -> {
                    System.out.println("Enter " + getName() + ": " + DragAssist.createArc);
                    if(DragAssist.createArc != null && DragAssist.createArc.within != within
                       || DragAssist.createNode != null && DragAssist.createNode.hasPorts()) {
                        evt.acceptTransferModes(TransferMode.ANY);
                        view.getStyleClass().add("good");
                        evt.consume();
                    }
                });
                view.setOnDragDropped(evt -> {
                    view.getStyleClass().remove("good");
                    if(DragAssist.createArc != null) {
                        DragAssist.createArc.connectTo(this);
                        getContext().adjustArcs();
                    } else if(DragAssist.createNode != null) {
                        System.out.println("Create node");
                        DragAssist.targetX = evt.getScreenX();
                        DragAssist.targetY = evt.getScreenY();
                        getContext().setHovered(this);
                        Node n = getContext().make(DragAssist.createNode);
                        if(n.hasPorts()) {
                            connectTo(n.defaultPort(!in));
                        }
                        getContext().layoutAction();
                    }
                    evt.setDropCompleted(true);
                    evt.consume();
                });
                view.setOnDragExited(evt -> {
                    view.getStyleClass().remove("good");
                    evt.consume();
                });
                /*
//            } else  {
                view.setOnDragEntered(evt -> {
                    System.out.println("Out Enter " + ae.meta.name + ": " + DragAssist.createArc);
                    if(DragAssist.createNode != null && DragAssist.createNode.hasInputs()) {
                        evt.acceptTransferModes(TransferMode.ANY);
                        view.getStyleClass().add("good");
                        evt.consume();
                    }
                });
                view.setOnDragDropped(evt -> {
                    view.getStyleClass().remove("good");
                    if(DragAssist.createNode != null) {
                        System.out.println("Create node");
                        DragAssist.targetX = evt.getScreenX();
                        DragAssist.targetY = evt.getScreenY();
                        outa.container.controller.hovered = outa;
                        com.aws.jag.fxnodeeditor.graph.FGNode n = outa.container.controller.make(DragAssist.createNode);
                        n.defaultIn().setIncoming(outa);
                        within.controller.layoutAction();
                    }
                    evt.setDropCompleted(true);
                    evt.consume();
                });
                view.setOnDragOver(evt -> {
                    if(DragAssist.createNode != null) {
                        evt.acceptTransferModes(TransferMode.ANY);
                        evt.consume();
                    }
                });
                */
//        }
    }
    public Point2D getPosition(Transform area) {
        try {
            var nv = (NodeView) within;
            var box = nv.isExpanded() ? getView() : nv.getView();
            var lbl = box.getBoundsInLocal();
            var t = box.getLocalToSceneTransform();
            var ret = t.transform(lbl.getMinX(), lbl.getHeight() / 2);
            if(!isRightSide()) ret = ret.add(lbl.getWidth(), 0);
            return area.inverseTransform(ret);
        } catch(NonInvertibleTransformException ex) {
            Dlg.error("getPosition error", ex);
            return new Point2D(0,0);
        }
    }
    @Override
    public void setValue(Object v) {
        super.setValue(v);
        setViewText();
    }
    public void setViewText() {
        view.setText(!metadata.isRightSide() || isConnected() ? metadata.getName() : metadata.getName()+"="+getValue());
    }
    public void reposition(Transform t) {
        forEachArc(a->((ArcView)a).reposition(t));
    }
    public javafx.scene.Node getView() { return view; }
    @Override
    public String toString() {
        return super.toString() + ":" + view.getText();
    }
    
    private static final Image cursor = new Image(PortView.class.getResourceAsStream("DragTargetCursor.png"));
    static final private Image rightArrow = new Image(PortView.class.getResourceAsStream("RightArrow.png"));
}
