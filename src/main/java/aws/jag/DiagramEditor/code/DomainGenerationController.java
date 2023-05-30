/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.jag.DiagramEditor.code;

import aws.jag.DiagramEditor.nodegraph.*;
import java.util.*;


public interface DomainGenerationController {
    default void prescan(Collection<Object> messages){}
    public String generate(List<Node> nodes, CodeTarget target);
    default void close(){}
    default CodeTarget makeOutput(Domain d) { return new CodeTarget(d); }
}
