package test.scala

import cucumber.api.CucumberOptions
import cucumber.api.junit.Cucumber
import org.junit.runner.RunWith

@RunWith(classOf[Cucumber])
@CucumberOptions(features = Array("classpath:features/MyFeature.feature"),
  tags = Array("not @Wip"), glue = Array("classpath:lecture.examples.steps"),
  plugin = Array("pretty", "html:target/cucumber/html"))
class RunCucumberTests
