/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code;

import aws.WhiskeyJack.nodegraph.*;
import java.nio.file.*;
import java.util.*;

public interface OuterBuildController {
    public void handleDomains(Collection<OneDomain> domains);
    public Path getGenerationRootDirectory();
    public Path getCodePartDirectory(Domain d, String part);
    public Graph getWholeGraph();
    public void message(Object... m);
    public void error(Object... m);
    public void generateJavaPartBuild(Path dir, String mainclass, Collection<String> dependencies);
    public void runBuiltCode();
}
