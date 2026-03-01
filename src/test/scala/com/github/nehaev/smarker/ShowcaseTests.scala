package com.github.nehaev.smarker

import com.github.nehaev.smarker.template
import com.github.nehaev.smarker.templates
import utest._
import utest.framework.Formatter

object ShowcaseTests extends TestSuite {

    override val utestFormatter = new Formatter() {
        override val formatTruncateHeight = 200
    }

    val tests = Tests {
        test("JavaSource") {

            final case class JavaType(
                    `package`: String,
                    name: String,
                    genericParams: List[SimpleJavaType],
            )

            final case class SimpleJavaType(
                    `package`: String,
                    name: String,
            )

            final case class JavaAnnotationParam(
                    name: String,
                    value: String,
            )

            final case class JavaAnnotation(
                    `type`: JavaType,
                    params: List[JavaAnnotationParam],
            )

            final case class JavaBuilder(
                    generateInterface: Boolean,
                    interfaces: List[JavaType],
            )

            final case class JavaField(
                    name: String,
                    `type`: JavaType,
                    annotations: List[JavaAnnotation],
            ) derives CanEqual

            final case class JavaImport(
                    `package`: String,
                    name: String,
            )

            final case class JavaData(
                    name: String,
                    `type`: JavaType,
                    annotations: List[JavaAnnotation],
                    superClass: Option[JavaType],
                    interfaces: List[JavaType],
                    fields: List[JavaField],
                    builder: Option[JavaBuilder],
                    imports: List[JavaImport],
            )

            val javaData = JavaData(
                name = "User",
                `type` = JavaType(
                    `package` = "com.example",
                    name = "User",
                    genericParams = Nil,
                ),
                annotations = List(
                    JavaAnnotation(
                        `type` = JavaType(
                            `package` = "javax.persistence",
                            name = "Entity",
                            genericParams = Nil,
                        ),
                        params = List(
                            JavaAnnotationParam(
                                name = "table",
                                value = "\"user\"",
                            ),
                            JavaAnnotationParam(
                                name = "schema",
                                value = "\"public\"",
                            ),
                        ),
                    )
                ),
                superClass = Some(
                    JavaType(
                        `package` = "com.example",
                        name = "BaseEntity",
                        genericParams = Nil,
                    )
                ),
                interfaces = List(
                    JavaType(
                        `package` = "com.example",
                        name = "Versioned",
                        genericParams = Nil,
                    )
                ),
                fields = List(
                    JavaField(
                        name = "id",
                        `type` = JavaType(
                            `package` = "java.lang",
                            name = "Long",
                            genericParams = Nil,
                        ),
                        annotations = List(
                            JavaAnnotation(
                                `type` = JavaType(
                                    `package` = "javax.persistence",
                                    name = "Id",
                                    genericParams = Nil,
                                ),
                                params = Nil,
                            )
                        ),
                    ),
                    JavaField(
                        name = "name",
                        `type` = JavaType(
                            `package` = "java.lang",
                            name = "String",
                            genericParams = Nil,
                        ),
                        annotations = Nil,
                    ),
                    JavaField(
                        name = "version",
                        `type` = JavaType(
                            `package` = "java.lang",
                            name = "Integer",
                            genericParams = Nil,
                        ),
                        annotations = Nil,
                    ),
                    JavaField(
                        name = "emails",
                        `type` = JavaType(
                            `package` = "java.util",
                            name = "List",
                            genericParams = List(
                                SimpleJavaType(
                                    `package` = "java.lang",
                                    name = "String",
                                )
                            ),
                        ),
                        annotations = Nil,
                    ),
                ),
                builder = Some(
                    JavaBuilder(
                        generateInterface = false,
                        interfaces = List(
                            JavaType(
                                `package` = "com.example",
                                name = "Builder",
                                genericParams = Nil,
                            )
                        ),
                    )
                ),
                imports = List(
                    JavaImport(
                        `package` = "com.example",
                        name = "BaseEntity",
                    ),
                    JavaImport(
                        `package` = "com.example",
                        name = "Builder",
                    ),
                    JavaImport(
                        `package` = "com.example",
                        name = "Versioned",
                    ),
                    JavaImport(
                        `package` = "java.util",
                        name = "List",
                    ),
                    JavaImport(
                        `package` = "javax.persistence",
                        name = "Entity",
                    ),
                    JavaImport(
                        `package` = "javax.persistence",
                        name = "Id",
                    ),
                ),
            )

            test("record header") {
                val templateSet = templates(
                    template[JavaData]("""
                        package [=type.package];

                        [#list items=imports sep="\n" /]

                        [#list items=annotations sep="\n" /]
                        public record [=name][#block start="(\n" end=")"]
                            [#list items=fields sep=",\n" /]
                        [/#block]
                        [#ifDefined value=superClass as="s"] extends [=s][/#ifDefined]
                        [#list items=interfaces start=" implements " end=" " sep=", " as="i"][=i][/#list]
                        [#block start="{\n" end="}"]
                            [#ifDefined value=builder as="b"]

                                public static class [=name]Builder [#list items=b.interfaces start="implements " end=" " sep=", " /][#block start="{\n" end="}"]

                                    [#list items=fields sep="" as="f"]
                                        private [=f.type] [=f.name];
                                    [/#list]

                                    [#list items=fields sep="" as="f"]
                                        public [=name]Builder [=f.name]([=f.type] [=f.name]) [#block start="{\n" end="}"]
                                            this.[=f.name] = [=f.name];
                                            return this;
                                        [/#block]

                                    [/#list]

                                    public [=name] build() [#block start="{\n" end="}"]
                                        return new [=name][#block start="(\n" end=")"]
                                            [#list items=fields sep=",\n" as="f"][=f.name][/#list]
                                        [/#block];
                                    [/#block]

                                [/#block]

                            [/#ifDefined]
                        [/#block]
                        """.trim()),
                    template[JavaField]("""[#list items=annotations sep="\n" end="\n" /][=type] [=name]"""),
                    template[JavaType]("""[=name][#list items=genericParams start="<" end=">" sep=", " /]"""),
                    template[SimpleJavaType]("""[=name]"""),
                    template[JavaImport]("""import [=package].[=name];"""),
                    template[JavaAnnotationParam]("""[=name] = [=value]"""),
                    template[JavaAnnotation]("""@[=type.name][#list items=params start="(" end=")" sep=", " /]"""),
                )

                val expected = """
                    |package com.example;
                    |
                    |import com.example.BaseEntity;
                    |import com.example.Builder;
                    |import com.example.Versioned;
                    |import java.util.List;
                    |import javax.persistence.Entity;
                    |import javax.persistence.Id;
                    |
                    |@Entity(table = "user", schema = "public")
                    |public record User(
                    |    @Id
                    |    Long id,
                    |    String name,
                    |    Integer version,
                    |    List<String> emails
                    |) extends BaseEntity implements Versioned {
                    |
                    |    public static class UserBuilder implements Builder {
                    |
                    |        private Long id;
                    |        private String name;
                    |        private Integer version;
                    |        private List<String> emails;
                    |
                    |        public UserBuilder id(Long id) {
                    |            this.id = id;
                    |            return this;
                    |        }
                    |        public UserBuilder name(String name) {
                    |            this.name = name;
                    |            return this;
                    |        }
                    |        public UserBuilder version(Integer version) {
                    |            this.version = version;
                    |            return this;
                    |        }
                    |        public UserBuilder emails(List<String> emails) {
                    |            this.emails = emails;
                    |            return this;
                    |        }
                    |
                    |        public User build() {
                    |            return new User(
                    |                id,
                    |                name,
                    |                version,
                    |                emails);
                    |        }
                    |    }
                    |}
                """.trim().stripMargin

                val actual = templateSet.flatMap(_.render(javaData)).left.map(println).toOption.get
                assert(actual == expected)
            }

        }
    }

}
