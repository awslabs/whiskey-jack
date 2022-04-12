/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import static com.nighthacks.fxnodeeditor.util.Utils.*;
import java.lang.reflect.*;
import java.util.*;
import javafx.scene.control.*;
import javafx.scene.layout.*;

public class Dlg {
    public static void error(Object... o) {
        var body = new VBox();
        Dialog d = new Dialog();
        var dp = d.getDialogPane();
        d.setTitle("Error");
        dp.setContent(body);
        addStuff(body, o);
        dp.getButtonTypes().add(ButtonType.OK);
        d.showAndWait();
    }
    private static void addStuff(VBox v, Object o) {
        if(o == null)
            return;
        if(o.getClass().isArray()) {
            int len = Array.getLength(o);
            for(var i = 0; i < len; i++)
                addStuff(v, Array.get(o, i));
        } else
            switch(o) {
                case null -> {
                }
                case Throwable t -> {
                    var u = getUltimateCause(t);
                    var m = u.getMessage();
                    if(isEmpty(m)) m = u.toString();
                    addStuff(v,m);
                    addStuff(v,u.getStackTrace());
                }
                case Collection c ->
                    c.forEach(e -> addStuff(v, e));
                default -> {
                    var s = deepToString(o, 80).toString();
                    if(!isEmpty(s)) {
                        var c = v.getChildren();
                        if(c.size() < 10)
                            c.add(new Label(s));
                    }
                }
            }
    }

}
