/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.graph;

import com.aws.jag.fxnodeeditor.meta.*;
import java.util.*;

public class OutArc extends ArcEndpoint {
    OutArc(Port m, FGNode c) {
        super(m, c);
    }
    final ArrayList<InArc> connectsTo = new ArrayList<>();
    @Override
    public void setViewText() {
        var v = getView();
        if(v != null)
            v.setText(meta.name);
    }
}
