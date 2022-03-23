/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import java.util.*;

public class OutArc extends ArcEndpoint {
    OutArc(MNode.Port m, FGNode c) {
        super(m, c);
    }
    final ArrayList<InArc> goesTo = new ArrayList<>();
}
