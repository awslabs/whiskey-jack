/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import com.nighthacks.fxnodeeditor.meta.*;
import java.io.*;
import javafx.scene.input.*;

public class DragAssist {
    public static final DataFormat draggingMetaNode = new DataFormat("meta drop");
    public static final DataFormat draggingOutArc = new DataFormat("node arc");
    public static final Object dummy = new Serializable() {
    };
    public static OutArc createArc;
    public static MetaNode createNode;
    public static double targetX;
    public static double targetY;
    public static void dragClean() {
        createArc = null;
        createNode = null;
    }
}
