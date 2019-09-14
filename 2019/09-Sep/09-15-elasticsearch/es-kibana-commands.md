### Kibana Developer Tools Commands

```
# Index a new document in the meta index of type author with an ID of 1.
PUT /author/_doc/1
{
  "first": "Andrew",
  "last": "Jarombek",
  "age": 24,
  "state": "CT",
  "town": "Riverside",
  "profession": "Software Engineer",
  "employers": [
    "Gartner"
  ]
}

# Retrieve the author document just created.
GET /author/_doc/1
GET /author/_doc/1?pretty=true

# Delete the author document just created.
DELETE /author/_doc/1

# Delete the entire author index and its corresponding type.
DELETE /author

# Create the settings for an index and a mapping for its type.
PUT /author
{ ... }

# Get the mapping of the type in the author and license_plate indexes.
GET /author/_mapping
GET /license_plate/_mapping

# Retrieve all documents from all indexes.  Returns 10 (or less) documents by default.
GET /_search

# Retrieve all documents in the license_plate index.
GET /license_plate/_search

# Retrieve documents in the license_plate index which conform this match query.
GET /license_plate/_search
{ ... }
```