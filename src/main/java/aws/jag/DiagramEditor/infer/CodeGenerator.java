/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.infer;

import aws.jag.DiagramEditor.nodegraph.*;
import aws.jag.DiagramEditor.nodeviewerfx.*;
import java.util.*;
import java.util.function.*;

public class CodeGenerator implements Scanner {
    @Override
    public <T extends Graph> void Scan(T g) {
        Dlg.error("Too boken to demo");
        var arcs = new ArrayList<Arc>(20);
        g.forEachArc((Consumer<Arc>) a -> arcs.add(a));
    }
}
