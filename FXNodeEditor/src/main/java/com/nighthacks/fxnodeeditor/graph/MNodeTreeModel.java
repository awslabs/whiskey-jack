/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.input.*;

public class MNodeTreeModel {
    static final Image cursor = new Image(FGNode.class.getResourceAsStream("CreateNodeCursor.png"));
    TreeView tree;
    NodeLibrary lib;
    public void initialize(TreeView t, NodeLibrary l) {
        tree = t;
        lib = l;
        t.setRoot(new nTreeItem(lib.root));
        t.setShowRoot(false);
        t.setCellFactory(i
                -> {
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
                    setOnDragDetected(evt->{
                        var db = startDragAndDrop(TransferMode.ANY);
                        System.out.println("DD "+evt);
                        var content = new ClipboardContent();
                        DragAssist.createNode = getItem();
                        content.put(DragAssist.draggingMetaNode, DragAssist.dummy);
                        db.setContent(content);
                        evt.consume();
                        db.setDragView(cursor);
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
