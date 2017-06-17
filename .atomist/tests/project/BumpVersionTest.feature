Feature: BumpVersion
  Maybe every project wants to express version in different places, see
  I could do env vars with the build etc etc. Or I could just keep it
  consistent with automation.


  Scenario: BumpVersion should edit a project correctly
    Given a project with the version in ElmParser.scala
    When the BumpVersion is run
    Then parameters were valid
    Then changes were made
    Then that version number looks different
    Then the version number is updated in the pom
