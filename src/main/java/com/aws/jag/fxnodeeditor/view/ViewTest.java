/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.*;
import javafx.scene.layout.*;

public class ViewTest {
    public static void annotate(AnchorPane nodeEditor) {
        var m = new MetaNode();
        m.name = "Add";
        m.description = "add two things together";
        m.domain = Domain.device;
        m.add(new MetaPort("in", null, m, Type.int_t, 0, true))
                .add(new MetaPort("b", null, m, Type.int_t, 0, true))
                .add(new MetaPort("out", null, m, Type.int_t, 0, false));
        var v = new View();
        var nv = v.newNode(m);
        var fx = nv.getView();
        fx.setLayoutX(50);
        fx.setLayoutY(50);
        nodeEditor.getChildren().add(fx);
    }

}
