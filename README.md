# Fennek CMS

# Syntax

## Base Types

There are 4 (5?) base types you can create. (`site`?), `context`, `component`, `page` and `store`. Every type can have unlimited properties defined
in it.

## Properties

Properties are defined with a key and a set of symbols, which define the type and behaviour of the property.

**Examples:**

```
class? = @String() @Default(value: "")
```

Defines a optional property with the name `class` of type String with a default value of `""`

```
title =
  @Ui(inline: true, initialValue: "")
  @String(min_length: 0, max_length: 50, regex: "/.*/g")
  @Default(value: "lorem")
```

Defines a required property with the name `title` of type String with a default value of `"lorem"`. The Symbol `@Ui`
defines the appearance of this proeprty within the ui of the cms. In this case, the title is inline editable and has an
initial value of `""`.

## Symbols

### `@Ui`

Defines the behavoiur of a property within the UI of the CMS. Available attributes are:

| Attribute      | Description                                                                                                               | Required | Default |
| -------------- | ------------------------------------------------------------------------------------------------------------------------- | -------- | ------- |
| `label`        | The label which should be used for the properties input. Only gets used, if the field is display in the @temp: Inspector. | false    | ""      |
| `inline`       | Defines if the property is inline editable or editable only within the @temp: Inspector.                                  | false    | false   |
| `initialValue` | Defines the value, which should be displayed in the ui, as soon as a new instance of the property is created              | false    | false   |

### `@Default`

Sets the default value of the property. If the value would be `null`, it gets assigned this value instead.

| Attribute | Description                                   | Required | Default |
| --------- | --------------------------------------------- | -------- | ------- |
| `value`   | The value which should be used as the default | true     |         |

### `@String`

Sets the type of the property to `string` and renders a single-line (@TODO: How to define a multiline field?) input
field in the CMS. You may provide the Symbol with the following additional attributes:

| Attribute    | Description                                                                             | Required | Default    |
| ------------ | --------------------------------------------------------------------------------------- | -------- | ---------- |
| `min_length` | The minimum length of the string                                                        | false    | 0          |
| `max_length` | The maximum length of the string                                                        | false    | `Infinity` |
| `regex`      | A regex, which has to create a match with the provided value @TODO: Which regex syntax? | false    |            |

### `@Number`

Sets the type of the property to `number` and renders a number input in the ui. You may provide the Symbol with the
following additional attributes:

| Attribute  | Description                                          | Required | Default    |
| ---------- | ---------------------------------------------------- | -------- | ---------- |
| `min`      | The minimum allowed value of the number              | false    | 0          |
| `max`      | The maximum allowed value of the number              | false    | `Infinity` |
| `fraction` | The number of fractionals the number gets rounded to | false    | 0          |

### `@Boolean`

Sets the type of the property to `boolean` and renders a checkbox in the ui. You may provide the Symbol with the
following additional attributes:

| Attribute | Description | Required | Default |
| --------- | ----------- | -------- | ------- |

### `@Selection`

Sets the type of the property to `string` and renders a select in the ui. You may provide the Symbol with the following
additional attributes:

| Attribute  | Description                                                                     | Required | Default |
| ---------- | ------------------------------------------------------------------------------- | -------- | ------- |
| `options`  | an array of `@Option` Symbols to be used a options                              | true     |         |
| `multiple` | A boolean flag, indicating, if the user may select multiple, or just one option | false    | false   |

#### `@Option`

The `@Option` Symbol may only be used inside the `options` attribute of the `@Selection` Symbol. You may provide the
Symbol with the following additional attributes:

| Attribute | Description                                                                               | Required | Default |
| --------- | ----------------------------------------------------------------------------------------- | -------- | ------- |
| `label`   | The label of the option, which is displayed inside the CMS UI.                            | true     |         |
| `value`   | The value of the option, which gets assiged to the property, if this option was selected. | true     |         |

### `@Url`

Sets the type of the property to `string` and renders a url field in the CMS. You may provide the Symbol with the
following additional attributes:

| Attribute  | Description                                                                                                      | Required | Default |
| ---------- | ---------------------------------------------------------------------------------------------------------------- | -------- | ------- |
| `external` | If the user may provide an external url. If this is set to false, the user may only select pages within the cms. | false    | true    |
| `mail`     | If the user may provide an email address to be used as a `mailto:` url                                           | false    | true    |
| `tel`      | If the user may provide a phone number to be used as a `tel:` url                                                | false    | true    |
| `asset`    | If the user may select an asset from the asset library (@TODO: How to specify, which mime-types are allowed)     | false    | true    |

### `@Asset`

Sets the type of the property to `Asset` and renders a asset selection in the CMS. You may provide the Symbol with the
following additional attributes:

| Attribute | Description                                                                               | Required | Default |
| --------- | ----------------------------------------------------------------------------------------- | -------- | ------- |
| `type`    | An array of allowed mime-types to be selected. Example: ["image/*"] to only allow images. | false    | ["*"]   |
| `upload`  | If the user is able to directly upload new assets.                                        | false    | true    |
| `library` | If the user is able to select assets from the library.                                    | false    | true    |

### `@QueryMany` and `@QuerySingle`

Queries the Content Tree with the specified Queries and sets the value of the property to the returned result. QueryMany
will always return an array of values or an empty array, if nothing is found. QuerySingle will return a single value or
null, if nothing is found. If an array is returned to QuerySingle, only the first item will be returned.

You may provide the Symbol with the following additional attributes:

| Attribute | Description                                                                           | Required | Default   |
| --------- | ------------------------------------------------------------------------------------- | -------- | --------- |
| `from`    | Specifies, from where the query should be executed. May be either `current` or `root` | false    | `current` |
| `queries` | An array of Query Symbols to define the behavoiur of the Query                        | true     |           |

#### `@Children`

The `@Children` Symbol may only be used inside the `queries` attribute of a `@Query*` Symbol. It returnes an array of
all (direct) children of the current node.

You may provide the Symbol with the following additional attributes:

| Attribute    | Description                                                                          | Required | Default |
| ------------ | ------------------------------------------------------------------------------------ | -------- | ------- |
| `instanceOf` | The custom type of the component or page to be returned (will return all by default) | false    |         |
| `slot`       | The name of the slot to search in. Will search in all by default                     | false    |         |
| `deep`       | If the search should go deeper recursively instead of just returning direct children | false    | false   |

#### `@Filter`

The `@Filter` Symbol may only be used inside the `queries` attribute of a `@Query*` Symbol. It has to be provided with a
condition and only keeps the nodes, which match the condition.

You may provide the Symbol with the following additional attributes:

| Attribute   | Description                               | Required | Default |
| ----------- | ----------------------------------------- | -------- | ------- |
| `condition` | The condition on which to match the nodes | true     |         |
