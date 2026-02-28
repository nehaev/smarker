package com.github.nehaev.smarker

import com.github.nehaev.smarker.template
import com.github.nehaev.smarker.templates
import utest._

object ShowcaseTests extends TestSuite {

    val tests = Tests {
        test("JavaSource") {

            final case class JavaType(
                    `package`: String,
                    name: String,
                    genericParams: List[JavaType],
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
                    generate: Boolean,
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
                        params = Nil,
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
                                JavaType(
                                    `package` = "java.lang",
                                    name = "String",
                                    genericParams = Nil,
                                )
                            ),
                        ),
                        annotations = Nil,
                    ),
                ),
                builder = Some(
                    JavaBuilder(
                        generate = true,
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
                        public record [=name][#block start="(\n" end="\n)"]
                            [#list items=fields sep=",\n" as="f" /]
                        [/#block] {}
                        """.trim()),
                    template[JavaField]("""
                        [=type.name][#list items=type.genericParams start="<" end=">" sep=", " as="g"][=g.name][/#list] [=name]
                        """.trim()),
                )

                val expected ="""
                    |public record User(
                    |    Long id,
                    |    String name,
                    |    Integer version,
                    |    List<String> emails
                    |) {}
                """.trim().stripMargin

                val actual = templateSet.flatMap(_.render(javaData)).toOption.get
                assert(actual == expected)
            }

        }
    }

}
