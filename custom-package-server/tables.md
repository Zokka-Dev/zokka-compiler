+ Packages table
    * id
    * name
    * version
    * location
    * hash
    * repository_id
+ Repositories table
    * id
    * humanReadableName
    * machineReadableName
+ Users table
    * id
    * username
    * passwordHash
    * repository_id
+ Auth tokens table
    * user_id
    * repository_id
    * permission_level
