/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package aws.WhiskeyJack.code;

import aws.WhiskeyJack.nodegraph.*;
import java.util.*;


public interface DomainGenerationController {
    default void prescan(){}
    public String generate(List<Node> nodes, CodeTarget target);
    default void close(){}
    CodeTarget makeOutput();
}
