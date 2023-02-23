/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.nodeviewerfx;

import aws.jag.DiagramEditor.nodegraph.MetaNode;
import java.io.*;
import javafx.scene.input.*;

public class DragAssist {
    public static final DataFormat draggingMetaNode = new DataFormat("meta drop");
    public static final DataFormat draggingOutArc = new DataFormat("node arc");
    public static final Object dummy = new Serializable() {
    };
    public static PortView createArc;
    public static MetaNode createNode;
    public static double targetX;
    public static double targetY;
    public static void dragClean() {
        createArc = null;
        createNode = null;
    }
}
