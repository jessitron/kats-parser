Feature: Lib lib libbits TreePrinter
  The TreePrinter libbit editor copies TreePrinter into your project.


  Scenario: TreePrinterLibbit should add sample files to the project
    Given an empty project
    When the TreePrinterLibbit is run
    Then changes were made
    Then the new TreePrinter source file exists
    Then the new TreePrinter test files exist

  Scenario: TreePrinterLibbit should do nothing when a file already exists
    Given the new TreePrinter source file already exists
    When the TreePrinterLibbit is run
    Then no changes were made