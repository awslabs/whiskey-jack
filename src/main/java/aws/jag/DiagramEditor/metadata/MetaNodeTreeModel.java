/*
 * SPDX-FileCopyrightText: Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.metadata;

import aws.jag.DiagramEditor.nodeviewerfx.GraphView;
import aws.jag.DiagramEditor.nodeviewerfx.NodeView;
import aws.jag.DiagramEditor.nodeviewerfx.DragAssist;
import aws.jag.DiagramEditor.metadataeditor.MetaEditorController;
import aws.jag.DiagramEditor.nodegraph.MetaNode;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.input.*;

public class MetaNodeTreeModel {
    static final Image cursor = new Image(NodeView.class.getResourceAsStream("CreateNodeCursor.png"));
//    TreeView tree;
//    GraphView nec;
    public void initialize(TreeView t, GraphView n) {
//        nec = n;
//        tree = t;
        t.setRoot(new nTreeItem(MetaNode.metaMeta));
        t.setShowRoot(false);
        t.setCellFactory(i -> {
            var ret = new TreeCell<MetaNode>() {
                @Override
                protected void updateItem(MetaNode item, boolean empty) {
                    super.updateItem(item, empty);
                    if(empty || item == null) {
                        setText(null);
                        setGraphic(null);
                    } else
                        setText(item.getName());
                }
                {
                    setEditable(false);
                    setOnDragDetected(evt -> {
                        DragAssist.dragClean();
                        var db = startDragAndDrop(TransferMode.ANY);
                        var content = new ClipboardContent();
                        DragAssist.createNode = getItem();
                        content.put(DragAssist.draggingMetaNode, DragAssist.dummy);
                        db.setContent(content);
                        evt.consume();
                        db.setDragView(cursor);
                    });
                    setOnMouseClicked((MouseEvent mouseEvent) -> {
                        if(mouseEvent.getButton().equals(MouseButton.PRIMARY))
                            if(mouseEvent.getClickCount() == 2)
                                MetaEditorController.edit(getItem(), n);
                    });
                }
            };
            return ret;
        });
    }
    private class nTreeItem extends TreeItem<MetaNode> {
        nTreeItem(MetaNode m) {
            super(m);
            m.forEachChild(c -> getChildren().add(new nTreeItem(c)));
        }
        @Override
        public String toString() {
            return getValue().getName();
        }
    }
//    static MetaNode dragSource;

}
