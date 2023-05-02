/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.jag.DiagramEditor.code;

import aws.jag.DiagramEditor.nodegraph.*;
import java.util.*;


public interface GenerateCode {
    default void prescan(){}
    public void generate(List<Node> nodes, CodeTarget target);
    default void close(){}
}
