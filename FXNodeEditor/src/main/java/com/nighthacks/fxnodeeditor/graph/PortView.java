/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.input.*;

public class PortView extends Label {
    private final FGNode fgnode;
    static final private Image rightArrow = new Image(FGNode.class.getResourceAsStream("RightArrow.png"));

    @SuppressWarnings(value="LeakingThisInConstructor")
    PortView(ArcEndpoint ae, final FGNode o) {
        super(ae.meta.name);
        fgnode = o;
        setGraphicTextGap(4);
        boolean in = ae.meta.in;
        getStyleClass().add(in ? "inPort" : "outPort");
        javafx.scene.image.ImageView img = new ImageView(rightArrow);
        img.setFitHeight(10);
        img.setPreserveRatio(true);
        img.setCache(true);
        img.setSmooth(true);
        setGraphic(img);
        localToSceneTransformProperty().addListener(b -> {
            fgnode.controller.adjustArcs();
        });
        setContentDisplay(in ? ContentDisplay.LEFT : ContentDisplay.RIGHT);
        switch(ae) {
            case InArc ea -> {
                setOnMouseClicked(evt -> {
                    javafx.scene.control.TextInputDialog td = new TextInputDialog(String.valueOf(ea.value));
                    td.setHeaderText(ea.meta.name);
                    td.setTitle("Enter new value");
                    ea.setValue(td.showAndWait());
                });
                setOnDragDropped(evt -> {
                    getStyleClass().remove("good");
                    if(DragAssist.createArc != null) {
                        ea.setIncoming(DragAssist.createArc);
                        fgnode.controller.adjustArcs();
                    } else if(DragAssist.createNode != null) {
                        System.out.println("Create node");
                        DragAssist.targetX = evt.getScreenX();
                        DragAssist.targetY = evt.getScreenY();
                        ea.container.controller.hovered = ea;
                        com.nighthacks.fxnodeeditor.graph.FGNode n = ea.container.controller.make(DragAssist.createNode);
                        if(!n.outputs.isEmpty())
                            ea.setIncoming(n.defaultOut());
                        fgnode.controller.layoutAction();
                    }
                    evt.setDropCompleted(true);
                    evt.consume();
                });
                setOnDragOver(evt -> {
                    if(DragAssist.createNode != null || DragAssist.createArc != null && DragAssist.createArc.container != fgnode) {
                        evt.acceptTransferModes(TransferMode.ANY);
                        evt.consume();
                    }
                });
                setOnDragEntered(evt -> {
                    System.out.println("Enter " + ae.meta.name + ": " + DragAssist.createArc);
                    if(DragAssist.createArc != null && DragAssist.createArc.container != fgnode || DragAssist.createNode != null && DragAssist.createNode.hasOutputs()) {
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
                        com.nighthacks.fxnodeeditor.graph.FGNode n = outa.container.controller.make(DragAssist.createNode);
                        n.defaultIn().setIncoming(outa);
                        fgnode.controller.layoutAction();
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
            default -> {
            }
        }
        ae.setView(this);
    }
    private static final Image cursor = new Image(FGNode.class.getResourceAsStream("DragTargetCursor.png"));
    @Override
    public String toString() {
        return super.toString() + ":" + getText();
    }

}
