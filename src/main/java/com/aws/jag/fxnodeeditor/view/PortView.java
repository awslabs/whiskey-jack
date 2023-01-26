/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.*;
//import com.aws.jag.fxnodeeditor.graph.*;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.transform.*;
import javax.annotation.*;

public class PortView extends Port {
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
    public GraphView getContext() {
        return (GraphView)super.getContext();
    }
    static final private Image rightArrow = new Image(PortView.class.getResourceAsStream("RightArrow.png"));
    private Label label;

    @SuppressWarnings(value="LeakingThisInConstructor")
    private void init() { 
        label = new Label("");
        setViewText();
        label.setGraphicTextGap(4);
        var in = metadata.in;
        label.getStyleClass().add(in ? "inPort" : "outPort");
        var img = new ImageView(rightArrow);
        img.setFitHeight(10);
        img.setPreserveRatio(true);
        img.setCache(true);
        img.setSmooth(true);
        label.setGraphic(img);
        label.localToSceneTransformProperty().addListener(b -> {
            getContext().adjustArcs();
        });
        label.setContentDisplay(in ? ContentDisplay.LEFT : ContentDisplay.RIGHT);
        /*
        if(in) {
                label.setOnMouseClicked(evt -> {
                    TextInputDialog td = new TextInputDialog(String.valueOf(constantValue));
                    td.setHeaderText(metadata.name);
                    td.setTitle("Enter new value");
                    setValue(td.showAndWait().get());
                });
                label.setOnDragDropped(evt -> {
                    label.getStyleClass().remove("good");
                    if(DragAssist.createArc != null) {
                        ea.setIncoming(DragAssist.createArc);
                        within.controller.adjustArcs();
                    } else if(DragAssist.createNode != null) {
                        System.out.println("Create node");
                        DragAssist.targetX = evt.getScreenX();
                        DragAssist.targetY = evt.getScreenY();
                        ea.container.controller.hovered = ea;
                        com.aws.jag.fxnodeeditor.graph.FGNode n = ea.container.controller.make(DragAssist.createNode);
                        if(!n.outputs.isEmpty())
                            ea.setIncoming(n.defaultOut());
                        within.controller.layoutAction();
                    }
                    evt.setDropCompleted(true);
                    evt.consume();
                });
                setOnDragOver(evt -> {
                    if(DragAssist.createNode != null || DragAssist.createArc != null && DragAssist.createArc.container != within) {
                        evt.acceptTransferModes(TransferMode.ANY);
                        evt.consume();
                    }
                });
                setOnDragEntered(evt -> {
                    System.out.println("Enter " + ae.meta.name + ": " + DragAssist.createArc);
                    if(DragAssist.createArc != null && DragAssist.createArc.container != within || DragAssist.createNode != null && DragAssist.createNode.hasOutputs()) {
                        evt.acceptTransferModes(TransferMode.ANY);
                        getStyleClass().add("good");
                        evt.consume();
                    }
                });
                setOnDragExited(evt -> {
                    getStyleClass().remove("good");
                    evt.consume();
                });
            } else  {
                setOnDragDetected(evt -> {
                    DragAssist.dragClean();
                    javafx.scene.input.Dragboard db = startDragAndDrop(TransferMode.ANY);
                    javafx.scene.input.ClipboardContent content = new ClipboardContent();
                    DragAssist.createArc = outa;
                    content.put(DragAssist.draggingOutArc, DragAssist.dummy);
                    db.setContent(content);
                    evt.consume();
                    db.setDragView(cursor);
                });
                setOnDragEntered(evt -> {
                    System.out.println("Out Enter " + ae.meta.name + ": " + DragAssist.createArc);
                    if(DragAssist.createNode != null && DragAssist.createNode.hasInputs()) {
                        evt.acceptTransferModes(TransferMode.ANY);
                        getStyleClass().add("good");
                        evt.consume();
                    }
                });
                setOnDragDropped(evt -> {
                    getStyleClass().remove("good");
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
                setOnDragOver(evt -> {
                    if(DragAssist.createNode != null) {
                        evt.acceptTransferModes(TransferMode.ANY);
                        evt.consume();
                    }
                });
        }
/* */
    }
    @Override
    public void setValue(Object v) {
        super.setValue(v);
        setViewText();
    }
    public void setViewText() {
        label.setText(!metadata.in || isConnected() ? metadata.getName() : metadata.getName()+"="+constantValue);
    }
    public void reposition(Transform t) {
        // TODO implement
        System.out.println("Reposition "+getFullName());
    }
    public javafx.scene.Node getView() { return label; }
    private static final Image cursor = new Image(PortView.class.getResourceAsStream("DragTargetCursor.png"));
    @Override
    public String toString() {
        return super.toString() + ":" + label.getText();
    }
}
