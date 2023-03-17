/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.infer;

import aws.jag.DiagramEditor.nodegraph.*;

public interface Scanner {
    public <T extends Graph> void Scan(T g);
}
