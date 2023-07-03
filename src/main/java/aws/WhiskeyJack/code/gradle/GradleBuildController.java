/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.code.gradle;

import aws.WhiskeyJack.code.*;
import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import static aws.WhiskeyJack.util.Exec.*;
import static aws.WhiskeyJack.util.Utils.*;
import java.io.*;
import java.nio.file.*;
import java.util.*;

@GeneratesCodeFor("/gradle/*")
public class GradleBuildController implements OuterBuildController,
        OverallCodeGenerationDriver.needsCodeGenerator {
    OverallCodeGenerationDriver context;
    private Path rootGenerationDirectory;
    private final Set<String> partNames = new HashSet<>();
    @Override
    public void setCodeGenerator(OverallCodeGenerationDriver cg) {
        context = cg;
    }
    @Override
    public void handleDomains(Collection<OneDomain> domains) {
        if(domains.isEmpty()) return;
        // March all the code domains through the various phases in parallel
        domains.forEach(d -> d.prescan());
        domains.forEach(d -> d.generate());
        domains.forEach(d -> d.close());
        try(var out = startPart("settings.gradle")) {
            out.append("rootProject.name = '")
                    .append("TBD")
                    .append("'\n");
            for(var n: partNames)
                out.append("include('")
                        .append(n)
                        .append("')\n");
        } catch(IOException ex) {
            error(ex);
        }
        try(var out = startPart("gradle.properties")) {
            out.append("netbeans.hint.jdkPlatform=JDK_17\n");
            var slot = 0;
            for(var p: partNames)
                out.append("action.custom-" + ++slot
                           + "=" + p + " run\n"
                           + "action.custom-" + slot
                           + ".args=--configure-on-demand -w -x check :"
                           + p + ":run\n");
        } catch(IOException ex) {
            error(ex);
        }
    }
    @Override
    public Graph getWholeGraph() {
        return context.getWholeGraph();
    }
    @Override
    public Path getGenerationRootDirectory() {
        if(rootGenerationDirectory == null) {
            var dir = context.getWholeGraph().getSrc().toString();
            if(dir == null) dir = deTilde("~/untitledade").toString();
            if(dir.endsWith(".ade")) dir = dir.substring(0, dir.length() - 4);
            var path = Path.of(dir + ".netbeans");
            try {
                Files.createDirectories(path);
            } catch(IOException ex) {
                /* ignore */ }
            rootGenerationDirectory = path;
        }
        return rootGenerationDirectory;
    }
    private Writer startPart(String name) throws IOException {
        return Files.newBufferedWriter(getGenerationRootDirectory().resolve(name));
    }
    @Override
    public Path getCodePartDirectory(Domain d, String part) {
        var name = d.getName();
        if(!part.isEmpty())
            name = name + '_' + part;
        partNames.add(name);
        var nm = getGenerationRootDirectory().resolve(name);
        try {
            Files.createDirectories(nm);
        } catch(IOException ex) {
            /* ignore */ }
        return nm;
    }
    @Override
    public void message(Object... m) {
        context.message(m);
    }
    @Override
    public void error(Object... m) {
        context.error(m);
    }
    @Override
    public void generateJavaPartBuild(Path dir, String mainclass, Collection<String> dependencies) {
        try(var build = Files.newBufferedWriter(dir.resolve("build.gradle"),
                StandardOpenOption.CREATE,
                StandardOpenOption.TRUNCATE_EXISTING)) {
            build.append(
                    """
                    plugins {
                        id 'application'
                    }
                    repositories {
                        mavenCentral()
                        mavenLocal()
                    }
                    java {
                        toolchain {
                            languageVersion = JavaLanguageVersion.of(17)
                        }
                    }
                    tasks.named('test') {
                        useJUnitPlatform()
                    }
                    dependencies {
                        testImplementation 'org.junit.jupiter:junit-jupiter:5.9.1'
                    """);
            for(var d: dependencies)
                build.append("    implementation '")
                        .append(d)
                        .append("'\n");
            build.append("}\n");
            if(!isEmpty(mainclass))
                build.append("application {\n  mainClass = '")
                        .append(mainclass)
                        .append("'\n}\n");

        } catch(IOException ex) {
            error(ex);
        }
    }
    Exec exec;
    @Override
    public void runBuiltCode() {
        if(exec!=null) {
            message("Previous execution terminated");
            exec.close();
        }
        exec = new Exec();
        try {
            exec.cd(rootGenerationDirectory.toFile())
                    .withExec("/bin/sh", "-c", "pkill -f ade.gen; pwd; printenv; /usr/local/bin/gradle run") // TODO: make this work on windows
                    .withErr(System.out::append)
                    .withOut(System.out::append);
            System.out.println(exec + "; WHICH " + exec.which("gradle"));
            exec.background(xc -> {
                message("Run ended with code " + xc);
            });
            message("App is running in the background");
        } catch(Throwable ex) {
            error("Couldn't run app", ex);
        }
    }
}
