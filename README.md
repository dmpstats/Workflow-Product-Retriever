
# Workflow Product Retriever

MoveApps

Github repository:
<https://github.com/dmpstats/Workflow-Products-Retriever>

## Description

Retrieves and appends Products from Apps in concurrent Workflows to the
current input data set via the MoveApps API, effectively enabling the
integration between multiple Workflows.

## Documentation

This App allows users to retrieve objects created in other Workflows and
append them to the App’s input data, permitting data exchanges among
concurrent Workflows. It uses MoveApps’ API functionality in creating
stable HTTP links for accessing up-to-date App Products, such as
artifacts and output files.

In practice, this App retrieves a single Product generated by a specific
App within an active instance of a concurrent Workflow. The downloaded
object is subsequently attached to the input `move2` data as an `R`
object attribute named `appended_products` (see [Output
data](#output-data) for further details).

Users can employ multiple instances of this App, chaining them
sequentially or deploying them at various points within the current
Workflow, to collect multiple Products from other workflows. Retrieved
objects are consecutively appended to the input’s `appended_products`
attribute as list elements.

<div>

> **Important**
>
> In order to use this functionality, the user must first create stable
> API links to the Workflow instance containing the desired Product(s).
> Instructions for generating API access credentials for your Workflows
> are available in the [MoveApps API Links
> guide](https://docs.moveapps.org/#/API).

</div>

Using the Workflow instance API access credentials, users are required
to specify the name of the target Product, along with either the title
or the position of the App within Workflow where it is located. Please
be aware that not all types of files can be retrieved. Currently
supported Product file-types are: **`.csv`**, **`.txt`** and **`.rds`**.

### Input data

A `move2` object.

### Output data

A `move2` object.

As mentioned above in [Documentation](#documentation), objects stored in
each retrieved target Product are appended to the input `move2` data as
an object attribute called `appended_products`. This forms the App’s
Output data.

The `appended_products` attribute is a list object, allowing the
sequential attachment of additional Product objects each time the App is
used in the current Workflow. Furthermore, each list element under the
`appended_products` comprises a sub-list with two elements:

- `metadata`, holding high-level information about the appended Product
  (e.g.  original workflow and App titles, last time modified, etc.)
- `object`, storing the actual Product object

Appended objects in the Output data can be accessed via the function
`attr()`.

For example, to access the appended products in an output data object
obtained after applying this App twice to retrieve Products from two
different apps in a concurrent (mock) workflow, we could run the
following:

``` r
apnd_prods <- attr(app_output, "appended_products")
length(apnd_prods)
```

    [1] 2

Thus, to check the metadata of the first appended product:

``` r
apnd_prods |> 
  purrr::pluck(1) |> 
  purrr::pluck("metadata")
```

      workflow_title        instance_title appPositionInWorkflow
    1           Mock Workflow Instance 001                     2
                      appTitle       fileName   mimeType fileSize
    1 Add Local and Solar Time data_wtime.csv text/plain   172252
                       modifiedAt file_basename file_ext
    1 2023-11-01T15:46:49.336246Z    data_wtime      csv

Analogously, to visualize the metadata and actual data in the second
appended Product, we could run:

``` r
apnd_prods |> 
  purrr::pluck(2)
```

    $metadata
      workflow_title        instance_title appPositionInWorkflow
    1           mock Workflow Instance 001                     8
                                                appTitle         fileName
    1 Standardise Formats and Calculate Basic Statistics summarystats.csv
        mimeType fileSize                  modifiedAt file_basename file_ext
    1 text/plain      411 2023-11-01T15:57:32.968009Z  summarystats      csv

    $object
    # A tibble: 2 × 12
       ...1 ID2           first_obs           last_obs            total_obs max_kmph
      <dbl> <chr>         <dttm>              <dttm>                  <dbl>    <dbl>
    1     1 Bateleur_8889 2023-10-01 14:45:12 2023-10-03 17:30:39       120     16.9
    2     2 TAWNY_8891    2023-10-01 14:45:12 2023-10-03 17:30:12       130     29.6
    # ℹ 6 more variables: mean_kmph <dbl>, med_kmph <dbl>, max_gap_mins <dbl>,
    #   max_alt <lgl>, min_alt <lgl>, total_km <dbl>

### Artefacts

`appended_product_metadata.csv`: a table with the metadata of the
appended workflow Product.

### Parameters

**ID of Target Workflow Instance** (`usr`): the API’s ID (i.e. Username)
of the Workflow Instance containing the target Product (check
<https://docs.moveapps.org/#/API>). Default: `NULL`.

**Password of Target Workflow Instance** (`pwd`): the API’s Password of
the Workflow Instance containing the target Product (check
<https://docs.moveapps.org/#/API>). Default: `NULL`.

**Target Workflow Title** (`workflow_title`): the name of the Workflow
with the target Product. While we recommend using the full title for
clarity, aliases or acronyms are accepted. Default: `NULL`.

**Target App Title** (`app_title`): the name of the App comprising the
target Product. Please ensure the name of the App is accurate. Not
required if ‘Target App Position in Workflow’ is specified. Default:
`NULL`.

**Target App Position in Workflow** (`app_pos`): Enter the position of
the App containing the target Product in the Workflow’s pipeline. Not
required if ‘Target App Title’ is specified. Default: `NULL`.

**Target Product Filename** (`product_file`): the target Product name.
Please ensure the filename is accurate. You can omit the extension,
unless multiple artifact files share the same basename. Currently
supported target Product file-types: ‘.rds’, ‘.csv’, and ‘.txt’.
Default: `NULL`.

### Most common errors

If any of the inputs specified for the parameters fail to contribute to
the correct identification of the target Product for retrieval, the App
will generate informative error messages pointing to where the
misspecification occurred. The most common misspecifications include:

- inaccurate naming of target App and/or Product names,
- absence of intended target Products in specified target App,
- inconsistency between the specified name of the target App and the
  specified position of the App in the target workflow

<div>

> **Caution**
>
> The App’s base code was developed based on the existing structure and
> attribute names underlying MoveApps’s API framework. If there are
> changes to naming conventions or alterations in the way API links are
> constructed in the future, the code will become susceptible to errors
> in HTTP requests.

</div>

### Null or error handling

Parameters `usr`, `pwd` and `product_file` must be specified, whereas at
least one of `app_title` and `app_pos` needs to be provided.

The `workflow_title` parameter is optional since workflow identification
is already provided by `usr`. However, we strongly recommend entering an
clear and identifiable name for the target workflow to ensure clarity
when using appended Products in downstream Apps.
