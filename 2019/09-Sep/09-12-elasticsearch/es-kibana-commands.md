### Kibana Developer Tools Commands

```
# Index a new document in the meta index of type author with an ID of 1.
PUT /meta/author/1
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
GET /meta/author/1

# Delete the author document just created.
DELETE /meta/author/1
```