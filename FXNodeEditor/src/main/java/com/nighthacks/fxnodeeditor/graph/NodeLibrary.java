/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import com.nighthacks.fxnodeeditor.*;
import com.nighthacks.fxnodeeditor.util.*;
import java.io.*;
import java.nio.file.*;
import javafx.event.*;

public class NodeLibrary {
    private NodeEditorController nec;
    private Path root;
    public void exportAction(ActionEvent t) {
        try( CommitableWriter out = CommitableWriter.abandonOnClose(root.resolve("total.mn"))) {
            NodeEditorController.fileio.writeValue(out, Collectable.asObject(MNode.root));
            out.commit();
        } catch(IOException ioe) {
            Dlg.error("Can't save file",ioe);
        }
    }
    public void initialize(NodeEditorController n) {
        nec = n;
        root = Config.scanConfig("mn", (a, b) -> System.out.println("MN " + a + " " + b));
    }
}
