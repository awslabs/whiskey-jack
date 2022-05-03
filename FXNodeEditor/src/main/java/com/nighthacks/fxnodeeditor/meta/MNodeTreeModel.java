/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.meta;

import com.nighthacks.fxnodeeditor.graph.*;
import com.nighthacks.fxnodeeditor.metaedit.*;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.input.*;

public class MNodeTreeModel {
    static final Image cursor = new Image(FGNode.class.getResourceAsStream("CreateNodeCursor.png"));
    TreeView tree;
    NodeLibrary lib;
    NodeEditorController nec;
    public void initialize(TreeView t, NodeLibrary l, NodeEditorController n) {
        nec = n;
        tree = t;
        lib = l;
        t.setRoot(new nTreeItem(lib.root));
        t.setShowRoot(false);
        t.setCellFactory(i -> {
            var ret = new TreeCell<MNode>() {
                @Override
                protected void updateItem(MNode item, boolean empty) {
                    super.updateItem(item, empty);
                    if(empty || item == null) {
                        setText(null);
                        setGraphic(null);
                    } else
                        setText(item.name);
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
    class nTreeItem extends TreeItem<MNode> {
        nTreeItem(MNode m) {
            super(m);
            if(m.children != null)
                m.children.values().forEach(c -> getChildren().add(new nTreeItem(c)));
        }
        @Override
        public String toString() {
            return getValue().name;
        }
    }
    static MNode dragSource;

}
